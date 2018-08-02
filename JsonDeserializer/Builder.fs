module JsonDeserializer.Builder

open FSharpx.Collections
open JsonDeserializer.Types
open JsonStream.StateOps

type Production =
  | ValueArray  of nodes: JsonNode list
  | CommaArray  of nodes: JsonNode list
  | KeyObject   of nodes: Map<string, JsonNode> * key: string
  | ColonObject of nodes: Map<string, JsonNode> * key: string
  | ValueObject of Map<string, JsonNode>
  | CommaObject of Map<string, JsonNode>
  | Root        of JsonNode option

let emptyArr =
  ValueArray List.empty

let emptyObj =
  ValueObject Map.empty

let add x c = function
| CommaArray items ->
  ValueArray (x :: items) |> Ok
| ColonObject (items, k) ->
  ValueObject (Map.add k x items) |> Ok
| Root None ->
  x |> Some |> Root |> Ok
| _ -> unexpectedInput c |> Error
