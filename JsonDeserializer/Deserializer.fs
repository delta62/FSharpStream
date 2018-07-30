module JsonDeserializer.Deserializer

open FSharpx.Collections
open JsonStream.StateOps

(*
  DO care about stack size
  DO NOT care about constant memory usage

  Inputs:
  - null
  - true
  - "cheese"
  - 3.14159
  - [ 42 ]
  - [[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[ ...
  - { "foo": 42 }
  - { "very": { "nested": { "object": ... } } }

  "Steps":
  - Parse a scalar
  - Parse a [
  - In an array:
    - parse a ,
  - Parse a { "key":
  - In an object:
    - parse a , "key":

  So the context is a stack of
  - Some root state (empty stack?)
  - Partial objects. Map of 0 or more elements + optionally 1 key without a val
  - Partial arrays. List of 0 or more elements + a flag for a comma ([ 1, ] is invalid)
*)

let unexpectedEof = {
  Line = 0u;
  Column = 0u;
  Message = "Unexpected EOF";
}

let emptyDoc = {
  Line = 1u;
  Column = 1u;
  Message = "Empty JSON document"
}

type JsonNode =
  | Null
  | Boolean of bool
  | String  of string
  | Number  of string
  | Array   of JsonNode list
  | Object  of Map<string, JsonNode>

type TokenList = LazyList<Result<JsonVal<Token>, ParseError>>

type private JsonContext =
  | ArrayBuilder  of items: JsonNode list         * sawComma: bool
  | ObjectBuilder of items: Map<string, JsonNode> * key: string option
  | RootBuilder   of item:  JsonNode option

type ContextList = JsonContext list
type StepResult = Result<ContextList * TokenList, ParseError>

let scalar = function
| Token.Null     -> JsonNode.Null
| Token.True     -> JsonNode.Boolean true
| Token.False    -> JsonNode.Boolean false
| Token.String s -> JsonNode.String s
| Token.Number n -> JsonNode.Number n
| x              -> sprintf "Cannot convert non-scalar value %A" x |> failwith

let arrComma c ctx =
  match List.tryHead ctx with
  | Some (ArrayBuilder (items, comma)) when comma ->
    Ok (ArrayBuilder (items, true) :: List.tail ctx)
  | _ -> Error (unexpectedInput c)

let pushArr c ctx =
  match List.tryHead ctx with
  | Some (ArrayBuilder (items, _)) ->
    let nocomma = ArrayBuilder (items, false)
    let newArr = ArrayBuilder (List.empty, false)
    Ok (newArr :: nocomma :: List.tail ctx)
  | Some (ObjectBuilder (_, k)) when Option.isSome k ->
    let newArr = ArrayBuilder (List.empty, false)
    Ok (newArr :: ctx)
  | Some (RootBuilder None) ->
    Ok ([ ArrayBuilder (List.empty, false) ])
  | _ -> Error (unexpectedInput c)

let pushObj c ctx =
  match List.tryHead ctx with
  | Some (ArrayBuilder (items, _)) ->
    let nocomma = ArrayBuilder (items, false)
    let newObj = ObjectBuilder (Map.empty, None)
    Ok (newObj :: nocomma :: List.tail ctx)
  | Some (ObjectBuilder (_, k)) when Option.isSome k ->
    let newObj = ObjectBuilder (Map.empty, None)
    Ok (newObj :: ctx)
  | None ->
    Ok ([ ObjectBuilder (Map.empty, None) ])
  | _ -> Error (unexpectedInput c)

let addScalar c ctx =
  match List.tryHead ctx with
  | Some (ArrayBuilder (items, _)) ->
    let s = scalar c.Val
    Ok (ArrayBuilder (s :: items, false))
  | Some (ObjectBuilder (items, k)) ->
    match k, c.Val with
    | None, Token.String k ->
      Ok (ObjectBuilder (items, Some k))
    | Some k, c ->
      let s = scalar c
      let m = Map.add k s items
      Ok (ObjectBuilder (m, None))
    | None, _ ->
      Error (unexpectedInput c)
  | Some (RootBuilder None) ->
    let s = scalar c.Val
    Ok (RootBuilder (Some s))
  | Some (RootBuilder (Some _)) ->
    Error (unexpectedInput c)
  | None -> failwith "Unexpected state - empty ctx stack"

let popArr c ctx =
  ()

let addLiteral (ctx: ContextList) (lit: JsonVal<Token>): Result<ContextList, ParseError> =
    match lit.Val with
    | Comma        -> arrComma lit ctx
    | LeftBracket  -> pushArr lit ctx
    | LeftCurly    -> pushObj lit ctx
    | RightBracket -> popArr lit ctx
    | RightCurly   -> popObj lit ctx
    | Token.String _
    | Token.Number _
    | True
    | False
    | Token.Null -> addScalar lit ctx
    | Colon      -> Error (unexpectedInput lit)
    | Whitespace _ -> Ok ctx

let step (ctx: ContextList) (list: TokenList): StepResult =
  match list.TryUncons with
  | Some (Ok h, t) -> addLiteral ctx h t
  | Some (Error e, _) -> Error e
  | None -> Error unexpectedEof

let go (list: TokenList): Result<JsonNode, ParseError> =
  let ctx = [ RootBuilder None ]
  let list = LazyList.fold addLiteral (Ok ctx) list
  match LazyList.isEmpty list with
  | true -> Error emptyDoc
  | false ->
    Ok ()
    // Repeatedly call "step" until list is empty. Then check for invalid ctx.
