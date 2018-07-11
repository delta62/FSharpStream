module JsonStream.Tokenizer

open FSharpx.Collections
open RState
open JsonStream.StateOps
open Numbers
open Strings

let private charsToJsonChars chars =
  let initialState = { Line = 1u; Column = 0u; Val = '0'; }
  let folder state item =
    match item with
    | '\n' -> { Line = state.Line + 1u; Column = 1u; Val = item; }
    | _    -> { Line = state.Line; Column = state.Column + 1u; Val = item; }
  LazyList.scan folder initialState chars |> LazyList.skip 1

let private tokenAtChar char scalar =
  {
    Line   = char.Line;
    Column = char.Column;
    Token  = scalar;
  }

let private trueToken t =
  rstate {
    do! expectChars [ 'r'; 'u'; 'e'; ]
    return tokenAtChar t True
  }

let private falseToken f =
  rstate {
    do! expectChars [ 'a'; 'l'; 's'; 'e'; ]
    return tokenAtChar f False
  }

let private nullToken n =
  rstate {
    do! expectChars [ 'u'; 'l'; 'l'; ]
    return tokenAtChar n Null
  }

let private token =
  rstate {
    let! jc = nextChar
    let f = tokenAtChar jc
    return!
      match jc.Val with
      | '{' -> f LeftCurly    |> unit
      | '}' -> f RightCurly   |> unit
      | '[' -> f LeftBracket  |> unit
      | ']' -> f RightBracket |> unit
      | ':' -> f Colon        |> unit
      | ',' -> f Comma        |> unit
      | 't' -> trueToken jc
      | 'f' -> falseToken jc
      | 'n' -> nullToken jc
      | '"' -> stringToken |> fmap f
      | c when numericLeader c ->
        fmap f (numericToken jc)
      | _   -> unexpectedInput jc |> fail
  }

let tokenize chars =
  let unfolder s =
    match s with
    | LazyList.Nil -> None
    | _ ->
      let result = runStateR token s
      match result with
      | Ok (content, tail) -> Some (Ok content, tail)
      | Error e            -> Some (Error e, LazyList.empty)

  chars |> charsToJsonChars |> LazyList.unfold unfolder
