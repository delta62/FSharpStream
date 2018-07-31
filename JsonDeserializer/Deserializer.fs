module JsonDeserializer.Deserializer

open FSharpx.Collections
open JsonStream.StateOps
open JsonDeserializer.Builder

let emptyDoc = {
  Line = 1u;
  Column = 1u;
  Message = "Empty JSON document"
}

let scalar = function
| Token.Null     -> JsonNode.Null
| Token.True     -> JsonNode.Boolean true
| Token.False    -> JsonNode.Boolean false
| Token.String s -> JsonNode.String s
| Token.Number n -> JsonNode.Number n
| x              -> sprintf "Cannot convert non-scalar value %A" x |> failwith

let arrComma c = function
| ArrayBuilder (items, comma) :: xs when comma ->
  ArrayBuilder (items, true) :: xs |> Ok
| _ -> unexpectedInput c |> Error

let pushArr c = function
| ArrayBuilder (items, _) :: xs ->
  emptyArr :: ArrayBuilder (items, false) :: xs |> Ok
| (ObjectBuilder (_, k) :: _) as ctx when Option.isSome k ->
  emptyArr :: ctx |> Ok
| RootBuilder None :: _ ->
  [ emptyArr; RootBuilder None; ] |> Ok
| _ -> unexpectedInput c |> Error

let pushObj c = function
| ArrayBuilder (items, _) :: xs ->
  emptyObj :: ArrayBuilder (items, false) :: xs |> Ok
| (ObjectBuilder (_, Some _) :: _) as ctx ->
  emptyObj :: ctx |> Ok
| RootBuilder None :: _ ->
  [ emptyObj;  RootBuilder None; ] |> Ok
| _ -> unexpectedInput c |> Error

let addScalar c = function
| ArrayBuilder (items, _ ) :: xs ->
  ArrayBuilder (scalar c.Val :: items, false) :: xs |> Ok
| ObjectBuilder (items, k) as o :: xs ->
  match k, c.Val with
  | None, Token.String k ->
    ObjectBuilder (items, Some k) :: xs |> Ok
  | Some _, t ->
    Result.map (fun x -> x :: xs) (add (scalar t) c o)
  | None, _ -> unexpectedInput c |> Error
| RootBuilder None :: xs ->
  RootBuilder (Some (scalar c.Val)) :: xs |> Ok
| _ -> unexpectedInput c |> Error

let popArr c = function
| ArrayBuilder (items, comma) :: p :: xs when not comma ->
  let node = List.rev items |> JsonNode.Array
  Result.map (fun x -> x :: xs) (add node c p)
| _ -> unexpectedInput c |> Error

let popObj c = function
| ObjectBuilder (items, None) :: p :: xs ->
  let node = JsonNode.Object items
  Result.map (fun x -> x :: xs) (add node c p)
| _ -> unexpectedInput c |> Error

let addLiteral ctx token =
  Result.bind (fun ctx ->
    match token.Val with
    | Token.String _
    | Token.Number _
    | True
    | False
    | Token.Null   -> addScalar token ctx
    | Comma        -> arrComma  token ctx
    | LeftBracket  -> pushArr   token ctx
    | LeftCurly    -> pushObj   token ctx
    | RightBracket -> popArr    token ctx
    | RightCurly   -> popObj    token ctx
    | Colon        -> Error (unexpectedInput token)
    | Whitespace _ -> Ok ctx) ctx

let unwrapCtx = function
| [ RootBuilder (Some n) ] -> Ok n
| _                        -> Error emptyDoc

let deserialize list =
  let ctx = [ RootBuilder None; ]
  LazyList.fold addLiteral (Ok ctx) list
  |> Result.bind unwrapCtx
