module Tokenizer

open FSharpx.Collections
open System.IO
open State
open System.Text

type TokenType =
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

type JsonChar = {
  Line   : uint32;
  Column : uint32;
  Char   : char;
}

type JsonToken = {
  Line   : uint32;
  Column : uint32;
  Value  : TokenType;
}

type JsonStream = LazyList<JsonChar>

type ParseError = {
  Line    : uint32;
  Column  : uint32;
  Message : string;
}

let lazyUtf8Stream (stream: Stream) =
  use reader = new StreamReader(stream, Encoding.UTF8, false)
  let unfolder (reader: StreamReader) =
    if reader.EndOfStream then None
    else Some (reader.Read() |> char, reader)
  LazyList.unfold unfolder reader

let charsToCodePoints chars =
  let initialState = { Line = 1u; Column = 0u; Char = '0'; }
  let folder (state: JsonChar) item =
    match item with
    | '\n' -> { Line = state.Line + 1u; Column = 1u; Char = item; }
    | _    -> { Line = state.Line; Column = state.Column + 1u; Char = item; }
  LazyList.scan folder initialState chars

let fromIoStream stream =
   stream
    |> lazyUtf8Stream
    |> charsToCodePoints

let unexpectedEof =
  {
    Line    = 0u;
    Column  = 0u;
    Message = "Unexpected EOF";
  }

let unexpectedInput (char: JsonChar) =
  {
    Line    = char.Line;
    Column  = char.Column;
    Message = sprintf "Unexpected input: %c" char.Char;
  }

let unescaped = function
  | c when c >= 0x20 && c <= 0x21     -> true
  | c when c >= 0x23 && c <= 0x5B     -> true
  | c when c >= 0x5D && c <= 0x10FFFF -> true
  | _                                 -> false

let nextChar =
  state {
    let! (state: LazyList<JsonChar>) = get
    let (ret, tail) =
      match LazyList.isEmpty state with
      | true  -> Error unexpectedEof, LazyList.empty
      | false -> Ok state.Head, state.Tail
    do! put tail
    return ret
  }

let expectChar expected =
  state {
    let! actual = nextChar
    return Result.bind (fun x ->
      match x.Char with
      | c when c = expected -> Ok ()
      | _                   -> Error (unexpectedInput x)
    ) actual
  }

let eatChar =
  state {
    let! res = nextChar
    return Result.map ignore res
  }

let hexEsc =
  state {
    do! eatChar // u
    let! a = nextChar
    let! b = nextChar
    let! c = nextChar
    let! d = nextChar
    let num =
      ((a.Char |> uint32) <<< 24) +
      ((b.Char |> uint32) <<< 16) +
      ((c.Char |> uint32) <<< 8) +
      (d.Char  |> uint32)
    let char = num |> char
    return Ok ({ Line = a.Line; Column = a.Column; Char = char; })
  }

let esc =
  state {
    do! eatChar // "\"
    let! c = nextChar
    let! r =
      match c.Char with
      | '"'
      | '\\'
      | '/'
      | 'b'
      | 'f'
      | 'n'
      | 'r'
      | 't' -> Ok c |> unit
      | 'u' -> hexEsc
      | _   -> unexpectedInput c |> Error |> unit
    return r
  }

let str =
  state {
    let! nc = nextChar
    let! c =
      match nc.Char with
      | '\\' -> esc
      | '"'  -> nc |> Ok |> unit
      | c when unescaped (int c) -> nc |> Ok |> unit
      | _    -> unexpectedInput nc |> Error |> unit
    return Result.map (fun x -> String(sprintf "%c" x.Char)) c
  }

let tru =
  state {
    let! t = nextChar
    let! r = nextChar
    let! u = nextChar
    let! e = nextChar
    return Ok True
  }

let fls =
  state {
    let! f = nextChar
    let! a = nextChar
    let! l = nextChar
    let! s = nextChar
    let! e = nextChar
    return Ok False
  }

let nul =
  state {
    let! n = nextChar
    let! u = expectChar 'u'
    let! l = expectChar 'l'
    let! l = expectChar 'l'
    return Ok {
      Line   = n.Line;
      Column = n.Column;
      Value  = Null;
    }
  }

let num =
  state {
    return Number 42.0 |> Ok
  }

let numericLeader = function
| '-'                          -> true
| c when System.Char.IsDigit c -> true
| _                            -> false

let token =
  state {
    let! jc = nextChar
    let! r =
      match jc.Char with
      | '{' -> LeftCurly |> Ok |> unit
      | '}' -> RightCurly |> Ok |> unit
      | '[' -> LeftBracket |> Ok |> unit
      | ']' -> RightBracket |> Ok |> unit
      | ':' -> Colon |> Ok |> unit
      | ',' -> Comma |> Ok |> unit
      | '"' -> str
      | 't' -> tru
      | 'f' -> fls
      | 'n' -> nul
      | c when numericLeader c -> num
      | _ -> unexpectedInput jc |> Error |> unit
    return r
  }

let parse (stream: Stream) =
  let unfolder s =
    match s with
    | LazyList.Nil -> None
    | _            -> runState token s |> Some

  stream
   |> fromIoStream
   |> LazyList.unfold unfolder
