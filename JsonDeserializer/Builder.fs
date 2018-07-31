module JsonDeserializer.Builder

open FSharpx.Collections
open JsonStream.StateOps

type JsonNode =
  | Null
  | Boolean of bool
  | String  of string
  | Number  of string
  | Array   of JsonNode list
  | Object  of Map<string, JsonNode>

type JsonContext =
  | ArrayBuilder  of items: JsonNode list         * sawComma: bool
  | ObjectBuilder of items: Map<string, JsonNode> * key: string option
  | RootBuilder   of item:  JsonNode option

let emptyArr =
  ArrayBuilder (List.empty, false)

let emptyObj =
  ObjectBuilder (Map.empty, None)

let add x c = function
| ArrayBuilder (items, _) ->
  ArrayBuilder (x :: items, false) |> Ok
| ObjectBuilder (items, Some k) ->
  ObjectBuilder (Map.add k x items, None) |> Ok
| RootBuilder None ->
  RootBuilder (Some x) |> Ok
| _ -> unexpectedInput c |> Error
