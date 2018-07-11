module JsonStream.StateOps

open FSharpx.Collections
open RState

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
  | Number of string
  | Whitespace of string

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

type JsonVal<'a> = {
  Line   : uint32;
  Column : uint32;
  Val    : 'a;
}

let unexpectedInput char =
  {
    Line    = char.Line;
    Column  = char.Column;
    Message = sprintf "Unexpected input: %c" char.Val;
  }

let nextChar =
  RState (function
  | LazyList.Nil ->
    Error {
      Line    = 0u;
      Column  = 0u;
      Message = "Unexpected EOF";
    }
  | LazyList.Cons(h, t) -> Ok (h, t))

let putChar c =
  rstate {
    let! list = get
    do! LazyList.cons c list |> put
  }

let expectChar expected =
  rstate {
    let! next = nextChar
    return!
      match next.Val with
      | c when c = expected -> unit ()
      | _ -> unexpectedInput next |> fail
  }

let rec expectChars expected =
  rstate {
    match expected with
    | [ ] -> return ()
    | c :: tail ->
      do! expectChar c
      return! expectChars tail
  }

let expect f =
  rstate {
    let! next = nextChar
    return!
      match next.Val with
      | c when f c -> unit next
      | _ -> unexpectedInput next |> fail
  }

let maybeNextChar f =
  rstate {
    let! list = get
    if not (LazyList.isEmpty list) && f list.Head then
      let! jc = nextChar
      return Some jc
    else
      return None
  }

let takeWhile f =
  rstate {
    let! list = get
    let xs = LazyList.takeWhile f list
    let tail = LazyList.skip (List.length xs) list
    do! put tail
    return xs
  }
