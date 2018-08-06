module JsonSchema.Parser

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

let makeConstraint name node =
  match name with
  | "type"  -> makeTypeConstraint  node |> Some
  | "enum"  -> makeEnumConstraint  node |> Some
  | "const" -> makeConstConstraint node |> Some
  | _       -> None

let obj (m: Map<string, JsonNode>): Result<JsonSchema, string> =
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
