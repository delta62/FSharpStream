module JsonStream.TokenStream

open FSharpx.Collections
open JsonStream.RState
open JsonStream.StateOps

type JsonToken = Result<JsonVal<Token>, ParseError>
type JsonList = LazyList<JsonToken>

type JsonContext =
  | Root
  | Object
  | ObjectKey
  | ObjectColon
  | ObjectValue
  | Array
  | ArrayValue

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

let getContext: RState<JsonContext list, JsonContext, ParseError> =
  rstate {
    let! context = get
    match List.tryHead context with
    | Some head -> return head
    | None      -> return! invalidContext |> fail
  }

let pushContext ctx =
  rstate {
    let! context = get
    do! put (ctx :: context)
  }

let popContext: RState<JsonContext list, JsonContext, ParseError> =
  rstate {
    let! context = get
    match context with
    | [ ] ->
      return! invalidContext |> fail
    | x :: xs ->
      do! put xs
      return x
  }

let replaceCtx newctx =
  rstate {
    let! _ = popContext
    do! pushContext newctx
    return newctx
  }

let replaceCtxFn f newCtx =
  rstate {
    let! result = f
    let! _ = popContext
    do! pushContext newCtx
    return result
  }

let value next : RState<JsonContext list, JsonVal<Token>, ParseError> =
  rstate {
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

let object next =
  rstate {
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

let objectVal next =
  rstate {
    match next.Val with
    | RightCurly ->
      let! _ = popContext // ObjectVal
      let! _ = popContext // Object
      return next
    | Comma ->
      let! _ = popContext
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let array next =
  rstate {
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

let arrayVal next =
  rstate {
    match next.Val with
    | Comma ->
      let! _ = popContext // Pop ArrayVal
      return next
    | RightBracket ->
      let! _ = popContext // Pop ArrayVal
      let! _ = popContext // Pop Array
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let token tok =
  rstate {
    let! ctx = getContext
    match tok.Val with
    | Whitespace _ -> return tok
    | _ ->
      match ctx with
      | Root -> return! value tok
      | Object -> return! object tok
      | ObjectKey ->
        let! _ = replaceCtx ObjectColon
        return tok
      | ObjectColon ->
        let! v = value tok
        let! _ = replaceCtx ObjectValue
        return v
      | ObjectValue -> return! objectVal tok
      | Array -> return! array tok
      | ArrayValue -> return! arrayVal tok
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
    match LazyList.tryHead (fst state) with
    | None ->
      // Ensure that there is no unclosed context
      match snd state with
      | [ Root ] -> None
      | _   -> Some (Error unexpectedEof, (LazyList.empty, [ ]))
    | Some head ->
      match head with
      | Ok tok ->
        let result = runStateR (token tok) (snd state)
        match result with
        | Ok (token, list) -> Some (Ok token, ((LazyList.tail (fst state)), list))
        | Error e          -> Some (Error e, (LazyList.empty, [ ]))
      | Error e -> Some (Error e, (LazyList.empty, [ ]))

  let mappedList = LazyList.unfold unfolder (list, [ Root ])

  // Empty JSON documents are invalid.
  if emptyStream mappedList then
    LazyList.ofList [ Error unexpectedEof ]
  else
    mappedList
