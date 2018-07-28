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
  match h with
  | LeftBracket    -> obj t Map.empty
  | LeftCurly      -> arr t List.empty
  | Token.Null     -> Ok Null, t
  | True           -> Ok (Boolean true), t
  | False          -> Ok (Boolean false), t
  | Token.String s -> Ok (String s), t
  | Token.Number n -> Ok (Number n), t
  | _              -> Error "unexpected input", t

and obj l m =
  let h, t = LazyList.uncons l
  match h with
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
  | _ -> Error "unexpected input", t

and arr l a =
  let h, t = LazyList.uncons l
  match h with
  | Comma ->
    arr t a
  | RightBracket ->
    Ok (Array a), t
  | _ ->
    let v, t = value t
    match v with
    | Ok v -> arr t (v :: a)
    | Error e -> Error e, t

let deserialize l =
  let nows = function
  | { Val = Whitespace _ } -> false
  | _ -> true

  let v, t = l |> LazyList.filter nows |> LazyList.map (fun x -> x.Val) |> value

  match t with
  | LazyList.Nil -> v
  | LazyList.Cons _ -> Error "unexpected data after input"
