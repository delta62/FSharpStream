module JsonDeserializer.Deserializer

open FSharpx.Collections
open JsonStream
open JsonStream.StateOps
open JsonStream.Types
open JsonDeserializer.Builder
open JsonDeserializer.Types

let convertScalar = function
| Token.Null     -> JsonNode.Null
| Token.True     -> JsonNode.Boolean true
| Token.False    -> JsonNode.Boolean false
| Token.String s -> JsonNode.String s
| Token.Number n -> JsonNode.Number n
| x              -> sprintf "Cannot convert non-scalar value %A" x |> failwith

let flip f x y = f y x

let comma c = function
| ValueArray items :: xs when not (List.isEmpty items) ->
  CommaArray items :: xs |> Ok
| ValueObject items :: xs when not (Map.isEmpty items) ->
  CommaObject items :: xs |> Ok
| _ -> unexpectedInput c |> Error

let colon c = function
| KeyObject (items, k) :: xs ->
  ColonObject (items, k) :: xs |> Ok
| _ -> unexpectedInput c |> Error

let leftBracket c = function
| ValueArray items :: xs
| CommaArray items :: xs ->
  emptyArr :: ValueArray items :: xs |> Ok
| ValueObject items :: xs
| CommaObject items :: xs ->
  emptyArr :: ValueObject items :: xs |> Ok
| Root None :: _ ->
  [ emptyArr; Root None; ] |> Ok
| _ -> unexpectedInput c |> Error

let leftCurly c = function
| ValueArray items :: xs
| CommaArray items :: xs ->
  emptyObj :: ValueArray items :: xs |> Ok
| ValueObject items :: xs
| CommaObject items :: xs ->
  emptyObj :: ValueObject items :: xs |> Ok
| Root None :: _ ->
  [ emptyObj;  Root None; ] |> Ok
| _ -> unexpectedInput c |> Error

let scalar c = function
| ValueArray items :: xs
| CommaArray items :: xs ->
  ValueArray (convertScalar c.Val :: items) :: xs |> Ok
| ValueObject items :: xs
| CommaObject items :: xs ->
  match c.Val with
  | Token.String s -> KeyObject (items, s) :: xs |> Ok
  | _              -> unexpectedInput c |> Error
| ColonObject _ as o :: xs ->
  Result.map (flip List.cons xs) (add (convertScalar c.Val) c o)
| Root None :: xs ->
  Root (Some (convertScalar c.Val)) :: xs |> Ok
| _ -> unexpectedInput c |> Error

let rightBracket c = function
| ValueArray items :: p :: xs ->
  let node = List.rev items |> JsonNode.Array
  Result.map (flip (List.cons) xs) (add node c p)
| ValueArray _ :: _ ->
  failwith "Invalid state"
| _ -> unexpectedInput c |> Error

let rightCurly c = function
| ValueObject items :: p :: xs ->
  let node = JsonNode.Object items
  Result.map (flip List.cons xs) (add node c p)
| ValueObject _ :: _ ->
  failwith "Invalid state"
| _ -> unexpectedInput c |> Error

let addLiteral ctx token =
  Result.bind (fun ctx ->
    match token.Val with
    | Token.String _
    | Token.Number _
    | Token.True
    | Token.False
    | Token.Null         -> scalar token ctx
    | Token.Comma        -> comma token ctx
    | Token.LeftBracket  -> leftBracket token ctx
    | Token.LeftCurly    -> leftCurly token ctx
    | Token.RightBracket -> rightBracket token ctx
    | Token.RightCurly   -> rightCurly token ctx
    | Token.Colon        -> colon token ctx
    | Token.Whitespace _ -> Ok ctx) ctx

let unwrapCtx = function
| [ Root (Some n) ] -> Ok n
| _ ->
  let lastTok = { Line = 1u; Column = 1u; Val = Token.Whitespace " " }
  let state = { LastVal = lastTok; List = LazyList.empty }
  unexpectedEof state |> Error

let deserialize list =
  let ctx = [ Root None; ]
  LazyList.fold addLiteral (Ok ctx) list
  |> Result.bind unwrapCtx
