module JsonDeserializer.Deserializer

open FSharpx.Collections
open JsonStream
open JsonStream.StateOps
open JsonDeserializer.Builder

let convertScalar = function
| Token.Null     -> JsonNode.Null
| Token.True     -> JsonNode.Boolean true
| Token.False    -> JsonNode.Boolean false
| Token.String s -> JsonNode.String s
| Token.Number n -> JsonNode.Number n
| x              -> sprintf "Cannot convert non-scalar value %A" x |> failwith

let comma c = function
| ArrayBuilder (ValueArray items) :: xs when not (List.isEmpty items) ->
  ArrayBuilder (CommaArray items) :: xs |> Ok
| ObjectBuilder (ValueObject (items)) :: xs when not (Map.isEmpty items) ->
  ObjectBuilder (CommaObject (items)) :: xs |> Ok
| _ -> unexpectedInput c |> Error

let colon c = function
| ObjectBuilder (KeyObject (items, k)) :: xs ->
  ObjectBuilder (ColonObject (items, k)) :: xs |> Ok
| _ -> unexpectedInput c |> Error

let leftBracket c = function
| ArrayBuilder (ValueArray items) :: xs
| ArrayBuilder (CommaArray items) :: xs ->
  emptyArr :: ArrayBuilder (ValueArray items) :: xs |> Ok
| ObjectBuilder (ValueObject items) :: xs
| ObjectBuilder (CommaObject items) :: xs ->
  emptyArr :: ObjectBuilder (ValueObject items) :: xs |> Ok
| RootBuilder None :: _ ->
  [ emptyArr; RootBuilder None; ] |> Ok
| _ -> unexpectedInput c |> Error

let leftCurly c = function
| ArrayBuilder (ValueArray items) :: xs
| ArrayBuilder (CommaArray items) :: xs ->
  emptyObj :: ArrayBuilder (ValueArray items) :: xs |> Ok
| ObjectBuilder (ValueObject items) :: xs
| ObjectBuilder (CommaObject items) :: xs ->
  emptyObj :: ObjectBuilder (ValueObject items) :: xs |> Ok
| RootBuilder None :: _ ->
  [ emptyObj;  RootBuilder None; ] |> Ok
| _ -> unexpectedInput c |> Error

let scalar c = function
| ArrayBuilder (ValueArray items) :: xs
| ArrayBuilder (CommaArray items) :: xs ->
  ArrayBuilder (ValueArray (convertScalar c.Val :: items)) :: xs |> Ok
| ObjectBuilder (ValueObject items) :: xs
| ObjectBuilder (CommaObject items) :: xs ->
  match c.Val with
  | Token.String s ->
      ObjectBuilder (KeyObject (items, s)) :: xs |> Ok
  | _ -> unexpectedInput c |> Error
| ObjectBuilder (ColonObject _) as o :: xs ->
  Result.map (fun x -> x :: xs) (add (convertScalar c.Val) c o)
| RootBuilder None :: xs ->
  RootBuilder (Some (convertScalar c.Val)) :: xs |> Ok
| _ -> unexpectedInput c |> Error

let rightBracket c = function
| ArrayBuilder (ValueArray items) :: p :: xs ->
  let node = List.rev items |> JsonNode.Array
  Result.map (fun x -> x :: xs) (add node c p)
| ArrayBuilder (ValueArray _) :: _ ->
  failwith "Invalid state"
| _ -> unexpectedInput c |> Error

let rightCurly c = function
| ObjectBuilder (ValueObject items) :: p :: xs ->
  let node = JsonNode.Object items
  Result.map (fun x -> x :: xs) (add node c p)
| ObjectBuilder (ValueObject _) :: _ ->
  failwith "Invalid state"
| _ -> unexpectedInput c |> Error

let addLiteral ctx token =
  Result.bind (fun ctx ->
    match token.Val with
    | Token.String _
    | Token.Number _
    | True
    | False
    | Token.Null   -> scalar token ctx
    | Comma        -> comma  token ctx
    | LeftBracket  -> leftBracket   token ctx
    | LeftCurly    -> leftCurly   token ctx
    | RightBracket -> rightBracket    token ctx
    | RightCurly   -> rightCurly    token ctx
    | Colon        -> colon  token ctx
    | Whitespace _ -> Ok ctx) ctx

let unwrapCtx = function
| [ RootBuilder (Some n) ] -> Ok n
| _ ->
  let lastTok = { Line = 1u; Column = 1u; Val = Token.Whitespace " " }
  let state = { LastVal = lastTok; List = LazyList.empty }
  unexpectedEof state |> Error

let deserialize list =
  let ctx = [ RootBuilder None; ]
  LazyList.fold addLiteral (Ok ctx) list
  |> Result.bind unwrapCtx
