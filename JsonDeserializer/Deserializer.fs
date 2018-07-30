module JsonDeserializer.Deserializer

open FSharpx.Collections
open JsonStream.StateOps

type JsonNode =
  | Null
  | Boolean of bool
  | String  of string
  | Number  of string
  | Array   of JsonNode list
  | Object  of Map<string, JsonNode>

let rec value l =
  let h, t = LazyList.uncons l
  match h.Val with
  | LeftBracket    -> arr t List.empty
  | LeftCurly      -> obj t Map.empty
  | Token.Null     -> Ok Null, t
  | True           -> Ok (Boolean true), t
  | False          -> Ok (Boolean false), t
  | Token.String s -> Ok (String s), t
  | Token.Number n -> Ok (Number n), t
  | _              -> Error (unexpectedInput h), t

and obj l m =
  let h, t = LazyList.uncons l
  match h.Val with
  | Comma ->
    obj t m
  | RightCurly ->
    Ok (Object m), t
  | Token.String k ->
    let _, t = LazyList.uncons t // ":"
    let v, t = value t
    match v with
    | Ok v -> obj t (Map.add k v m)
    | Error e -> Error e, t
  | _ -> Error (unexpectedInput h), t

and arr l a =
  let h, t = LazyList.uncons l
  match h.Val with
  | Comma ->
    arr t a
  | RightBracket ->
    Ok (Array a), t
  | _ ->
    let v, t = value t
    match v with
    | Ok v -> arr t (v :: a)
    | Error e -> Error e, t

let isWhitespace = function
| { Val = Whitespace _ } -> true
| _ -> false

let deserialize l =
  let v, t = l |> LazyList.filter (isWhitespace >> not) |> value

  match t with
  | LazyList.Nil -> v
  | LazyList.Cons(h, _) ->
    Error {
      Line    = h.Line;
      Column  = h.Column;
      Message = sprintf "Unexpected data after input: %A" h.Val;
    }
