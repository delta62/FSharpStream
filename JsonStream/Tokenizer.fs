module Tokenizer

open FSharpx.Collections
open System.IO
open State
open System.Text

type Token =
  | LeftCurly
  | RightCurly
  | LeftBracket
  | RightBracket
  | Colon
  | Comma
  | True
  | False
  | Null
  | String of string
  | Number of double

type ParseError = {
  Line    : uint32;
  Column  : uint32;
  Message : string;
}

type JsonToken = {
  Line   : uint32;
  Column : uint32;
  Token  : Token;
}

type JsonChar = {
  Line   : uint32;
  Column : uint32;
  Char   : char;
}

type JsonStream = LazyList<JsonChar>

let lazyUtf8Stream (stream: Stream) =
  use reader = new StreamReader(stream, Encoding.UTF8, false)
  let unfolder (reader: StreamReader) =
    if reader.EndOfStream then None
    else Some (reader.Read() |> char, reader)
  LazyList.unfold unfolder reader

let charsToCodePoints chars =
  let initialState = { Line = 1u; Column = 0u; Char = '0'; }
  let folder state item =
    match item with
    | '\n' -> { Line = state.Line + 1u; Column = 1u; Char = item; }
    | _    -> { Line = state.Line; Column = state.Column + 1u; Char = item; }
  LazyList.scan folder initialState chars

let fromIoStream stream =
   stream |> lazyUtf8Stream |> charsToCodePoints

let unexpectedEof =
  {
    Line    = 0u;
    Column  = 0u;
    Message = "Unexpected EOF";
  }

let unexpectedInput char =
  {
    Line    = char.Line;
    Column  = char.Column;
    Message = sprintf "Unexpected input: %c" char.Char;
  }

let nextChar =
  RState (function
  | LazyList.Nil        -> Error unexpectedEof
  | LazyList.Cons(h, t) -> Ok (h, t))

let expectChar expected =
  RState (function
  | LazyList.Nil        -> Error unexpectedEof
  | LazyList.Cons(h, t) ->
    match h.Char with
    | c when c = expected -> Ok (h, t)
    | _                   -> Error (unexpectedInput h))

let expect f =
  RState (function
  | LazyList.Nil -> Error unexpectedEof
  | LazyList.Cons(h, t) ->
    match h.Char with
    | c when f c -> Ok (h, t)
    | _           -> Error (unexpectedInput h))

let eatChar =
  rstate {
    let! _ = nextChar
    return ()
  }

let (<<!) (c: char) w = uint32 c <<< w

let hexChar = function
| c when c >= 'a' && c <= 'z' -> true
| c when c >= 'A' && c <= 'z' -> true
| _                           -> false

let unicodeEsc =
  rstate {
    let! _ = expectChar 'u'
    let! a = expect hexChar
    let! b = expect hexChar
    let! c = expect hexChar
    let! d = expect hexChar
    let num = (a.Char <<! 24) + (b.Char <<! 16) + (c.Char <<!  8) + (d.Char |> uint32)
    let char = char num
    return { Line = a.Line; Column = a.Column; Char = char; }
  }

let fromChar jc c =
  {
    Line   = jc.Line;
    Column = jc.Column;
    Char   = c;
  }

let esc =
  rstate {
    let! c = nextChar
    let! r =
      match c.Char with
      | 'b' -> fromChar c '\b' |> unit
      | 'f' -> fromChar c '\f' |> unit
      | 'n' -> fromChar c '\n' |> unit
      | 'r' -> fromChar c '\r' |> unit
      | 't' -> fromChar c '\t' |> unit
      | '"'
      | '\\'
      | '/' -> unit c
      | 'u' -> unicodeEsc
      | _   -> unexpectedInput c |> fail
    return r
  }

let unescaped (x: char) =
  match uint32 x with
  | c when c >= 0x20u && c <= 0x21u     -> true
  | c when c >= 0x23u && c <= 0x5Bu     -> true
  | c when c >= 0x5Du && c <= 0x10FFFFu -> true
  | _                                   -> false

let tokenAtChar char scalar =
  {
    Line   = char.Line;
    Column = char.Column;
    Token  = scalar;
  }

let rec stringyThingy (s: LazyList<JsonChar>): Result<JsonChar list * LazyList<JsonChar>, ParseError> =
  let head = s.Head
  match head.Char with
  | '"' ->
    Ok ([ ], s.Tail)
  | c when unescaped c ->
    Result.map (fun (list, tail) -> head :: list, tail) (stringyThingy s.Tail)
  | '\\' ->
    Result.bind (fun (c, tail) ->
      Result.map (fun (list, tail2) -> c :: list, tail2) (stringyThingy tail)
    ) (runStateR esc s.Tail)
  | _ -> Error (unexpectedInput head)

let stringToken =
  rstate {
    let! nc = nextChar
    let! c =
      match nc.Char with
      | '\\'               -> esc
      | '"'                -> unit nc
      | c when unescaped c -> unit nc
      | _                  -> unexpectedInput nc |> fail
    return tokenAtChar c (String (sprintf "%c" c.Char))
  }

let trueToken t =
  rstate {
    let! _ = expectChar 'r'
    let! _ = expectChar 'u'
    let! _ = expectChar 'e'
    return tokenAtChar t True
  }

let falseToken f =
  rstate {
    let! _ = expectChar 'a'
    let! _ = expectChar 'l'
    let! _ = expectChar 's'
    let! _ = expectChar 'e'
    return tokenAtChar f False
  }

let nullToken n =
  rstate {
    let! _ = expectChar 'u'
    let! _ = expectChar 'l'
    let! _ = expectChar 'l'
    return tokenAtChar n Null
  }

let numericToken leader =
  rstate {
    return tokenAtChar leader (Number 42.0)
  }

let numericLeader = function
| '-'                          -> true
| c when System.Char.IsDigit c -> true
| _                            -> false

let token =
  rstate {
    let! jc = nextChar
    let! r =
      match jc.Char with
      | '{' -> tokenAtChar jc LeftCurly    |> unit
      | '}' -> tokenAtChar jc RightCurly   |> unit
      | '[' -> tokenAtChar jc LeftBracket  |> unit
      | ']' -> tokenAtChar jc RightBracket |> unit
      | ':' -> tokenAtChar jc Colon        |> unit
      | ',' -> tokenAtChar jc Comma        |> unit
      | '"' -> stringToken
      | 't' -> trueToken   jc
      | 'f' -> falseToken  jc
      | 'n' -> nullToken   jc
      | c when numericLeader c -> numericToken jc
      | _ -> unexpectedInput jc |> fail
    return r
  }

let parse (stream: Stream) =
  let unfolder s =
    match s with
    | LazyList.Nil -> None
    | _            -> runStateR token s |> Some

  stream
   |> fromIoStream
   |> LazyList.unfold unfolder
