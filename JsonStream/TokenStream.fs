module JsonStream.TokenStream

open FSharpx.Collections
open JsonStream.StateOps
open JsonStream.RState

type JsonContext = JsonRoot | JsonObject | JsonArray
type Stream = LazyList<JsonToken> * LazyList<JsonContext>

let tokenStream tokens =
  match tokens with
  | LazyList.Nil -> LazyList.ofArray [| Error { Line = 1u; Column = 1u; Message = "Input was empty"; } |]
  | LazyList.Cons(_) ->
    LazyList.empty

let unexpectedEof =
  {
    Line = 0u;
    Column = 0u;
    Message = "Unexpected eof"
  }

let expectedEof =
  {
    Line = 0u;
    Column = 0u;
    Message = "Expected end of input";
  }

let unexpectedToken =
  {
    Line = 0u;
    Column = 0u;
    Message = "Unexpected token"
  }

let pop: RState<Stream, JsonToken, ParseError> =
  rstate {
    let! stream, ctx = get
    match stream with
    | LazyList.Nil ->
      return! unexpectedEof |> fail
    | LazyList.Cons(h, t) ->
      do! put (t, ctx)
      return h
  }

let peek =
  rstate {
    let! stream, _ = get
    match stream with
    | LazyList.Nil -> return None
    | LazyList.Cons(h, _) -> return Some h.Token
  }

let pushCtx x =
  rstate {
    let! stream, ctx = get
    match ctx with
    | LazyList.Nil ->
      do! put (stream, LazyList.ofList [ x ])
      return ()
    | _ ->
      do! put (stream, LazyList.cons x ctx)
      return ()
  }

let rec consumeWhitespace =
  rstate {
    let! next = peek
    match next with
    | Some (Whitespace _) ->
      let! _ = pop
      return! consumeWhitespace
    | _ -> return ()
  }

let parseArray =
  rstate {
    return LazyList.empty
  }

let parseObject =
  rstate {
    return LazyList.empty
  }

let parseTopLevel =
  rstate {
    let! next = pop
    match next.Token with
    | LeftBracket ->
      do! pushCtx JsonArray
      return! parseArray
    | LeftCurly ->
      do! pushCtx JsonObject
      return! parseObject
    | Comma
    | Colon
    | RightBracket
    | RightCurly
    | Whitespace _ ->
      return! unexpectedToken |> fail
    | String _
    | Number _
    | True
    | False
    | Null ->
      return LazyList.ofList [ next ]
  }

let expectEof =
  rstate {
    let! next = peek
    match next with
    | None -> return ()
    | _    -> return! expectedEof |> fail
  }

let parseJson =
  rstate {
    do! consumeWhitespace
    let! item = parseTopLevel
    do! consumeWhitespace
    do! expectEof
    return item
  }
