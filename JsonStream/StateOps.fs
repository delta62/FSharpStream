module JsonStream.StateOps

open FSharpx.Collections
open JsonStream.RState
open JsonStream.Types

let unexpectedInput v = {
  Line    = v.Line;
  Column  = v.Column;
  Message = sprintf "Unexpected input: %A" v.Val;
}

let unexpectedEof state = {
  Line    = state.LastVal.Line;
  Column  = state.LastVal.Column + 1u;
  Message = "Unexpected end of input";
}

let peek<'a> : RState<TokenizerState<'a>, JsonVal<'a> option, ParseError> =
  rstate {
    let! s = get
    return LazyList.tryHead s.List
  }

let next<'a> : RState<TokenizerState<'a>, JsonVal<'a>, ParseError> =
  rstate {
    let! s = get
    match s.List with
    | LazyList.Nil ->
      return! unexpectedEof s |> fail
    | LazyList.Cons(h, t) ->
      do! put { LastVal = h; List = t; }
      return h
  }

let expect expected =
  rstate {
    let! next = next
    match next.Val with
    | c when c = expected ->
      return ()
    | _ ->
      return! unexpectedInput next |> fail
  }

let rec expectN expected =
  rstate {
    match expected with
    | [ ] -> return ()
    | c :: tail ->
      do! expect c
      return! expectN tail
  }

let expectWith f =
  rstate {
    let! next = next
    match next.Val with
    | c when f c ->
      return next
    | _ ->
      return! unexpectedInput next |> fail
  }

let maybeNextChar f : RState<TokenizerState<'a>, JsonVal<'a> option, ParseError> =
  rstate {
    let! state = get
    if not (LazyList.isEmpty state.List) && f state.List.Head then
      let! jc = next
      return Some jc
    else
      return None
  }

let takeWhile f : RState<TokenizerState<'a>, JsonVal<'a> list, ParseError> =
  rstate {
    let! s = get
    let xs = LazyList.takeWhile f s.List
    let tail = LazyList.skip (List.length xs) s.List

    match xs with
    | [ ] ->
      return xs
    | _ ->
      do! put { LastVal = List.last xs; List = tail; }
      return xs
  }
