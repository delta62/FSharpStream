module rec JsonSchema.Parser

open JsonDeserializer.Types
open JsonSchema.Types

let (<!>) = Result.map

let scalarTypeConstraint = function
| JsonNode.String "null"    -> ScalarType.Null    |> Ok
| JsonNode.String "boolean" -> ScalarType.Boolean |> Ok
| JsonNode.String "object"  -> ScalarType.Object  |> Ok
| JsonNode.String "array"   -> ScalarType.Array   |> Ok
| JsonNode.String "number"  -> ScalarType.Number  |> Ok
| JsonNode.String "string"  -> ScalarType.String  |> Ok
| JsonNode.String "integer" -> ScalarType.Integer |> Ok
| _                         -> Error "Invalid type constraint"

let rec foldResults list acc =
  match List.tryHead list with
  | Some (Ok x)->
    (fun xs -> x :: xs) <!> acc |> foldResults (List.tail list)
  | Some (Error e) -> Error e
  | None -> acc

let rec makeTypeConstraint node =
  match node with
  | JsonNode.Array xs ->
    foldResults (List.map scalarTypeConstraint xs) (Ok List.empty)
    |> Result.map (ListType >> Assertion.Type)
  | JsonNode.String _ ->
    Result.map ScalarType (scalarTypeConstraint node)
    |> Result.map Assertion.Type
  | _ -> Error "Invalid type constraint"

let makeEnumConstraint node =
  match node with
  | JsonNode.Array xs ->
    xs |> Assertion.Enum |> Ok
  | _ -> Error "Invalid enum constraint"

let makeConstConstraint node =
  Assertion.Const node |> Ok

let strToInt x =
  let couldParse, parsed = System.Int64.TryParse x
  match couldParse with
  | true  -> SchemaNumber.Integer parsed |> Ok
  | false -> sprintf "Unable to parse integer from %s" x |> Error

let strToUint x =
  let couldParse, parsed = System.UInt64.TryParse x
  match couldParse with
  | true  -> Ok parsed
  | false -> sprintf "Unable to parse unsigned integer from %s" x |> Error

let strToFloat x =
  let couldParse, parsed = System.Double.TryParse x
  match couldParse with
  | true -> SchemaNumber.Double parsed |> Ok
  | false -> sprintf "Unable to parse number from %s" x |> Error

let strToNum (x: string) =
  if x.Contains "." then
    strToFloat x
  else
    strToInt x

let makeMultipleOfConstraint = function
| JsonNode.Number n -> strToNum n |> Result.map Assertion.MultipleOf
| _ -> Error "Invalid multipleOf constraint"

let makeMaximumConstraint = function
| JsonNode.Number n -> strToInt n |> Result.map Assertion.Maximum
| _ -> Error "Invalid maximum constraint"

let makeMinimumConstraint = function
| JsonNode.Number n -> strToInt n |> Result.map Assertion.Minimum
| _ -> Error "Invalid minimum constraint"

let makeExclusiveMaximumConstraint = function
| JsonNode.Number n -> strToInt n |> Result.map Assertion.ExclusiveMaximum
| _ -> Error "Invalid exclusiveMaximumConstraint"

let makeExclusiveMinimumConstraint = function
| JsonNode.Number n -> strToInt n |> Result.map Assertion.ExclusiveMinimum
| _ -> Error "Invalid exclusiveMinimumConstraint"

let makeMaxLengthConstraint = function
| JsonNode.Number n -> strToUint n |> Result.map Assertion.MaxLength
| _ -> Error "Invalid maxLength"

let makeMinLengthConstraint = function
| JsonNode.Number n -> strToUint n |> Result.map Assertion.MinLength
| _ -> Error "Invalid minLength"

let makeAdditionalItemsConstraint node =
  Result.map Assertion.AdditionalItems (parse node)

let makeMaxItemsConstraint = function
| JsonNode.Number n -> strToUint n |> Result.map Assertion.MaxItems
| _ -> Error "Invalid maxItems"

let makeMinItemsConstraint = function
| JsonNode.Number n -> strToUint n |> Result.map Assertion.MinItems
| _ -> Error "Invalid minItems"

let makeUniqueItemsConstraint = function
| JsonNode.Boolean b -> b |> Assertion.UniqueItems |> Ok
| _ -> Error "Invalid uniqueItems"

let makeContainsConstraint node =
  Result.map Assertion.Contains (parse node)

let makeMaxPropertiesConstraint = function
| JsonNode.Number n -> strToUint n |> Result.map Assertion.MaxProperties
| _ -> Error "Invalid maxProperties"

let makeMinPropertiesConstraint = function
| JsonNode.Number n -> strToUint n |> Result.map Assertion.MinProperties
| _ -> Error "Invalid minProperties"

let makeRequiredConstraint = function
| JsonNode.Array xs ->
  // TODO unique
  List.fold (fun s x ->
    match x, s with
    | JsonNode.String s, Ok state -> Ok (s :: state)
    | _ -> Error "Invalid required"
  ) (Ok [ ]) xs |> Result.map Assertion.Required
| _ -> Error "Invalid required"

let makePropertiesConstraint = function
| JsonNode.Object m ->
  let folder s k v =
    match parse v, s with
    | Ok schema, Ok s -> Map.add k schema s |> Ok
    | _, Error e -> Error e
    | Error e, _ -> Error e
  Map.fold folder (Ok Map.empty) m |> Result.map Assertion.Properties
| _ -> Error "Invalid properties"

let makeAdditionalPropertiesConstraint node =
  Result.map Assertion.AdditionalProperties (parse node)

let makePropertyNamesConstraint node =
  Result.map Assertion.PropertyNames (parse node)

let makeItemsConstraint = function
| JsonNode.Array xs ->
  let folder s x =
    match s with
    | Error e -> Error e
    | Ok xs ->
      let schema = parse x
      Result.map (fun x -> x :: xs) schema
  let res = List.fold folder (Ok List.empty) xs
  Result.map (MultiJsonSchema >> Assertion.Items) res
| x ->
  let schema = parse x
  Result.map (SingletonItemSchema >> Assertion.Items) schema

let makeDependencyArray = function
| JsonNode.Array xs ->
  let folder state x =
    match x, state with
    | JsonNode.String s, Ok xs -> (Ok (s :: xs))
    | _ -> Error "Invalid dependency array item"
  let deps = List.fold folder (Ok List.empty) xs
  Result.map ArrayDependency deps
| _ -> Error "Invalid dependency array"

let makeSingletonDependency node =
  Result.map SchemaDependency (parse node)

let makeDependenciesConstraint = function
| JsonNode.Object m ->
  let folder s k v =
    match v, s with
    | JsonNode.Array _, Ok acc ->
      Result.map (fun x -> Map.add k x acc) (makeDependencyArray v)
    | x, Ok acc ->
      Result.map (fun x -> Map.add k x acc) (makeSingletonDependency x)
    | _ -> Error "Invalid dependency"
  let foo = Map.fold folder (Ok Map.empty) m
  Result.map Assertion.Depenedenciesof foo
| _ -> Error "Invalid dependencies"

let makeConstraint name node =
  match name with
  | "type"                 -> makeTypeConstraint node                 |> Some
  | "enum"                 -> makeEnumConstraint node                 |> Some
  | "const"                -> makeConstConstraint node                |> Some
  | "multipleOf"           -> makeMultipleOfConstraint node           |> Some
  | "maximum"              -> makeMaximumConstraint node              |> Some
  | "minimum"              -> makeMinimumConstraint node              |> Some
  | "exclusiveMaximum"     -> makeExclusiveMaximumConstraint node     |> Some
  | "exclusiveMinimum"     -> makeExclusiveMinimumConstraint node     |> Some
  | "maxLength"            -> makeMaxLengthConstraint node            |> Some
  | "minLength"            -> makeMinLengthConstraint node            |> Some
  // TODO pattern
  | "items"                -> makeItemsConstraint node                |> Some
  | "additionalItems"      -> makeAdditionalItemsConstraint node      |> Some
  | "maxItems"             -> makeMaxItemsConstraint node             |> Some
  | "minItems"             -> makeMinItemsConstraint node             |> Some
  | "uniqueItems"          -> makeUniqueItemsConstraint node          |> Some
  | "contains"             -> makeContainsConstraint node             |> Some
  | "maxProperties"        -> makeMaxPropertiesConstraint node        |> Some
  | "minProperties"        -> makeMinPropertiesConstraint node        |> Some
  | "required"             -> makeRequiredConstraint node             |> Some
  | "properties"           -> makePropertiesConstraint node           |> Some
  // TODO patternProperties
  | "additionalProperties" -> makeAdditionalPropertiesConstraint node |> Some
  | "dependencies"         -> makeDependenciesConstraint node         |> Some
  | "propertyNames"        -> makePropertyNamesConstraint node        |> Some
  | _                      -> None

let obj m =
  let init = (List.empty, List.empty) |> ObjectSchema |> Ok
  Map.fold (fun s k v ->
    let res = makeConstraint k v
    match res, s with
    | _, Error e        -> Error e
    | None, s           -> s
    | Some (Error e), _ -> Error e
    | Some (Ok x), Ok (ObjectSchema (xs, ys)) -> Ok (ObjectSchema (x :: xs, ys))
    // Should never be calling this with TrueSchema / FalseSchema
    | _                 -> Error "invalid state"
  ) init m

let parse = function
| JsonNode.Object m      -> obj m
| JsonNode.Boolean true  -> Ok TrueSchema
| JsonNode.Boolean false -> Ok FalseSchema
| _                      -> Error "Root level items must be objects"
