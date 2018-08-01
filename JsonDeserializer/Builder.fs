module JsonDeserializer.Builder

open FSharpx.Collections
open JsonStream.StateOps

type ArrayProduction =
  | ValueArray of nodes: JsonNode list
  | CommaArray of nodes: JsonNode list

type ObjectProduction =
  | KeyObject   of nodes: Map<string, JsonNode> * key: string
  | ColonObject of nodes: Map<string, JsonNode> * key: string
  | ValueObject of Map<string, JsonNode>
  | CommaObject of Map<string, JsonNode>

type JsonContext =
  | ArrayBuilder  of ArrayProduction
  | ObjectBuilder of ObjectProduction
  | RootBuilder   of JsonNode option

let emptyArr =
  ArrayBuilder (ValueArray List.empty)

let emptyObj =
  ObjectBuilder (ValueObject (Map.empty))

let add x c = function
| ArrayBuilder (CommaArray items) ->
  ArrayBuilder (ValueArray (x :: items)) |> Ok
| ObjectBuilder (ColonObject (items, k)) ->
  ObjectBuilder (ValueObject (Map.add k x items)) |> Ok
| RootBuilder None ->
  RootBuilder (Some x) |> Ok
| _ -> unexpectedInput c |> Error
