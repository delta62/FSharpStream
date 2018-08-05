module JsonSchema.Parser

open JsonDeserializer.Types
open JsonSchema.Types

let (<!>) = Result.map

let scalarTypeConstraint = function
| JsonNode.String "null"    -> Ok ScalarTypeAssertion.Null
| JsonNode.String "boolean" -> Ok ScalarTypeAssertion.Boolean
| JsonNode.String "object"  -> Ok ScalarTypeAssertion.Object
| JsonNode.String "array"   -> Ok ScalarTypeAssertion.Array
| JsonNode.String "number"  -> Ok ScalarTypeAssertion.Number
| JsonNode.String "string"  -> Ok ScalarTypeAssertion.String
| JsonNode.String "integer" -> Ok ScalarTypeAssertion.Integer
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
    ListType <!> foldResults (List.map scalarTypeConstraint xs) (Ok List.empty)
  | JsonNode.String _ -> ScalarType <!> scalarTypeConstraint node
  | _ -> Error "Invalid type constraint"

let makeEnumConstraint node =
  match node with
  | JsonNode.Array xs ->
    xs |> EnumAssertion |> Ok
  | _ -> Error "Invalid enum constraint"

let makeConstraint name node =
  match name with
  | "type" -> makeTypeConstraint node |> Some
  | "enum" -> makeEnumConstraint node |> Some
  | _      -> None

let obj (m: Map<string, JsonNode>): Result<JsonSchema, string> =
  let constraints = Map.fold (fun s k v -> s) List.empty m
  // map<string, JsonNode> -> Constraint list
  Error "not implemented"

let parse = function
| JsonNode.Object m      -> obj m
| JsonNode.Boolean true  -> Ok TrueSchema
| JsonNode.Boolean false -> Ok FalseSchema
| _                      -> Error "Root level items must be objects"
