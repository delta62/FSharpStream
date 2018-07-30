module JsonStream.Tokenizer

open FSharpx.Collections
open Character
open RState
open StateOps
open Numbers
open Strings
open System.Text

let private charsToJsonChars chars =
  let initialState = { Line = 1u; Column = 0u; Val = '0'; }
  let folder state item =
    match item with
    | '\n' -> { Line = state.Line + 1u; Column = 1u; Val = item; }
    | _    -> { Line = state.Line; Column = state.Column + 1u; Val = item; }
  LazyList.scan folder initialState chars |> LazyList.skip 1

let private tokenAtChar char scalar = {
  Line   = char.Line;
  Column = char.Column;
  Val    = scalar;
}

let private trueToken t =
  rstate {
    do! expectN [ 'r'; 'u'; 'e'; ]
    return tokenAtChar t True
  }

let private falseToken f =
  rstate {
    do! expectN [ 'a'; 'l'; 's'; 'e'; ]
    return tokenAtChar f False
  }

let private nullToken n =
  rstate {
    do! expectN [ 'u'; 'l'; 'l'; ]
    return tokenAtChar n Null
  }

let private whitespaceToken c =
  rstate {
    let! chars = takeWhile <| fun jc -> isWhitespace jc.Val
    return c :: chars
    |> List.map (fun jc -> jc.Val)
    |> List.toArray
    |> System.String
    |> Whitespace
    |> tokenAtChar c
  }

let private token =
  rstate {
    let! n = next
    let f = tokenAtChar n
    return!
      match n.Val with
      | '{' -> f LeftCurly    |> unit
      | '}' -> f RightCurly   |> unit
      | '[' -> f LeftBracket  |> unit
      | ']' -> f RightBracket |> unit
      | ':' -> f Colon        |> unit
      | ',' -> f Comma        |> unit
      | 't' -> trueToken n
      | 'f' -> falseToken n
      | 'n' -> nullToken n
      | '"' -> new StringBuilder() |> stringToken |> fmap f
      | c when numericLeader c ->
        fmap f (numericToken n)
      | c when isWhitespace c ->
        whitespaceToken n
      | _   -> unexpectedInput n |> fail
  }

let tokenize chars =
  let unfolder s =
    match s.List with
    | LazyList.Nil -> None
    | _ ->
      let result = runStateR token s
      match result with
      | Ok (content, newState) ->
        Some (Ok content, newState)
      | Error e ->
        Some (Error e, { s with List = LazyList.empty })

  let cs = chars |> charsToJsonChars
  let s = { Line = 0u; Column = 0u; Val = '\u0000'; }
  LazyList.unfold unfolder { LastVal = s; List = cs; }
