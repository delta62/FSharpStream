module JsonStream.TokenStream

open FSharpx.Collections
open JsonStream.RState
open JsonStream.StateOps

type JsonContext =
  | Root
  | Object
  | ObjectKey
  | ObjectColon
  | ObjectValue
  | Array
  | ArrayValue

type Stream = LazyList<JsonVal<Token>> * JsonContext list

let unexpectedEof = {
  Line    = 0u;
  Column  = 0u;
  Message = "Unexpected end of input";
}

let unexpectedInput input = {
  Line    = input.Line;
  Column  = input.Column;
  Message = sprintf "Unexpected input: %A" input;
}

let invalidContext = {
  Line = 0u;
  Column = 0u;
  Message = "invalid context"
}

let getToken: RState<Stream, JsonVal<Token>, ParseError> =
  rstate {
    let! list, context = get
    match list with
    | LazyList.Nil ->
      return! unexpectedEof |> fail
    | LazyList.Cons(h, t) ->
      do! put (t, context)
      return h
  }

let getContext: RState<Stream, JsonContext, ParseError> =
  rstate {
    let! _, context = get
    match List.tryHead context with
    | Some head -> return head
    | None      -> return! invalidContext |> fail
  }

let pushContext ctx =
  rstate {
    let! list, context = get
    do! put (list, ctx :: context)
  }

let popContext =
  rstate {
    let! list, context = get
    match context with
    | [ ] ->
      return! invalidContext |> fail
    | x :: xs ->
      do! put (list, xs)
      return x
  }

let replaceCtx expectedToken newctx =
  rstate {
    let! next = getToken
    match next with
    | x when x.Val = expectedToken ->
      let! _ = popContext
      do! pushContext newctx
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let replaceCtxFn f newCtx =
  rstate {
    let! result = f
    let! _ = popContext
    do! pushContext newCtx
    return result
  }

let value: RState<Stream, JsonVal<Token>, ParseError> =
  rstate {
    let! next = getToken
    match next.Val with
    | String _
    | Number _
    | True
    | False
    | Null ->
      return next
    | LeftCurly ->
      do! pushContext Object
      return next
    | LeftBracket ->
      do! pushContext Array
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let object =
  rstate {
    let! next = getToken
    match next.Val with
    | String _   ->
      do! pushContext ObjectKey
      return next
    | RightCurly ->
      let! _ = popContext
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let objectVal =
  rstate {
    let! next = getToken
    match next.Val with
    | RightCurly ->
      let! _ = popContext
      return next
    | Comma ->
      let! _ = popContext
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let array =
  rstate {
    let! next = getToken
    match next.Val with
    | String _
    | Number _
    | True
    | False
    | Null ->
      do! pushContext ArrayValue
      return next
    | LeftBracket ->
      do! pushContext Array
      return next
    | LeftCurly ->
      do! pushContext Object
      return next
    | RightBracket ->
      let! _ = popContext
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let arrayVal =
  rstate {
    let! next = getToken
    match next.Val with
    | Comma ->
      let! _ = popContext
      return next
    | RightBracket ->
      let! _ = popContext
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let token =
  rstate {
    let! list, ctx = get
    match list with
    | LazyList.Nil ->
      return! unexpectedEof |> fail
    | LazyList.Cons(h, _) ->
      match h.Val with
      | Whitespace _ ->
        let! _ = getToken
        return h
      | _ ->
        return!
          match List.head ctx with
          | Root        -> value
          | Object      -> object
          | ObjectKey   -> replaceCtx Colon ObjectColon
          | ObjectColon -> replaceCtxFn value ObjectValue
          | ObjectValue -> objectVal
          | Array       -> array
          | ArrayValue  -> arrayVal
  }

let emptyStream list =
  let finder = function
  | Ok ({ Val = Whitespace _ }) -> false
  | Ok _                        -> true
  | Error _                     -> false

  let result = LazyList.tryFind finder list

  match result with
  | Some _ -> false
  | None   -> true

let tokenStream list =
  let unfolder state =
    match fst state with
    | LazyList.Nil ->
      None
    | LazyList.Cons(_) ->
      let res = runStateR token state
      match res with
      | Ok (token, statePair) ->
        Some (Ok token, statePair)
      | Error e ->
        Some (Error e, (LazyList.empty, [ ]))

  let mappedList = LazyList.unfold unfolder (list, [ Root ])

  // Empty JSON documents are invalid
  if emptyStream mappedList then
    LazyList.ofList [ Error unexpectedEof ]
  else
    mappedList
