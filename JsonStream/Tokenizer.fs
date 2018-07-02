module Tokenizer

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

let charsToJsonChars chars =
  let initialState = { Line = 1u; Column = 0u; Char = '0'; }
  let folder state item =
    match item with
    | '\n' -> { Line = state.Line + 1u; Column = 1u; Char = item; }
    | _    -> { Line = state.Line; Column = state.Column + 1u; Char = item; }
  LazyList.scan folder initialState chars |> LazyList.skip 1

let unexpectedInput char =
  {
    Line    = char.Line;
    Column  = char.Column;
    Message = sprintf "Unexpected input: %c" char.Char;
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
      match next.Char with
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
      match next.Char with
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

let (<<!) (c: char) w = uint32 c <<< w

let hexChar = function
| c when System.Char.IsDigit c -> true
| c when c >= 'a' && c <= 'f'  -> true
| c when c >= 'A' && c <= 'F'  -> true
| _                            -> false

let unicodeEsc =
  rstate {
    let! a = expect hexChar
    let! b = expect hexChar
    let! c = expect hexChar
    let! d = expect hexChar
    let num = (a.Char <<! 24) + (b.Char <<! 16) + (c.Char <<!  8) + (d.Char |> uint32)
    return { Line = a.Line; Column = a.Column; Char = char num; }
  }

let esc =
  let fromChar jc c =
      unit { Line = jc.Line; Column = jc.Column; Char = c; }

  rstate {
    let! jc = nextChar
    return!
      match jc.Char with
      | 'b' -> fromChar jc '\b'
      | 'f' -> fromChar jc '\f'
      | 'n' -> fromChar jc '\n'
      | 'r' -> fromChar jc '\r'
      | 't' -> fromChar jc '\t'
      | '"'
      | '\\'
      | '/' -> unit jc
      | 'u' -> unicodeEsc
      | _   -> unexpectedInput jc |> fail
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

let rec stringToken acc =
  rstate {
    let! nc = nextChar
    match nc.Char with
    | '\\' ->
      let! c = esc
      return! stringToken (c.Char :: acc)
    | '"' ->
      let chars = List.rev acc |> Array.ofList |> System.String
      return String chars
    | c when unescaped c ->
      return! stringToken (c :: acc)
    | _ ->
      return! unexpectedInput nc |> fail
  }

let trueToken t =
  rstate {
    do! expectChars [ 'r'; 'u'; 'e'; ]
    return tokenAtChar t True
  }

let falseToken f =
  rstate {
    do! expectChars [ 'a'; 'l'; 's'; 'e'; ]
    return tokenAtChar f False
  }

let nullToken n =
  rstate {
    do! expectChars [ 'u'; 'l'; 'l'; ]
    return tokenAtChar n Null
  }

let rec digits acc =
  rstate {
    let! jc = maybeNextChar (fun jc -> System.Char.IsDigit jc.Char)
    match jc with
    | Some c -> return! digits (c.Char :: acc)
    | None   -> return List.rev acc
  }

let zeroOrDigits =
  rstate {
    let! next = expect System.Char.IsDigit
    match next.Char with
    | c when c = '0' -> return [ c ]
    | c -> return! digits [ c ]
  }

let frac =
  rstate {
    let! point = maybeNextChar (fun c -> c.Char = '.')
    match point with
    | Some _ ->
      let! x = expect System.Char.IsDigit
      let! xs = takeWhile (fun jc -> System.Char.IsDigit jc.Char)
      return List.concat [ [ '.' ]; [ x.Char ]; List.map (fun x -> x.Char) xs ]
    | None -> return [ ]
  }

let exp =
  rstate {
    let! e = maybeNextChar (fun c -> c.Char = 'e' || c.Char = 'E')
    match e with
    | Some jc ->
      let! signOpt = maybeNextChar (fun c -> c.Char = '+' || c.Char = '-')
      let sign =
        match signOpt with
        | Some jc -> [ jc.Char ]
        | None -> [ ]
      let! x = expect System.Char.IsDigit
      let! xs = takeWhile (fun jc -> System.Char.IsDigit jc.Char)
      return List.concat [ [ jc.Char ]; sign; [ x.Char ]; List.map (fun x -> x.Char) xs ]
    | None -> return [ ]
  }

let sign =
  rstate {
    let! sign = maybeNextChar (fun c -> c.Char = '-')
    match sign with
    | Some x -> return [ x.Char ]
    | None   -> return [ ]
  }

let numericToken leader =
  rstate {
    do! putChar leader
    let! sign = sign
    let! digits = zeroOrDigits
    let! frac = frac
    let! exp = exp

    return List.concat [ sign; digits; frac; exp; ]
      |> Array.ofList
      |> System.String
      |> Number
  }

let numericLeader = function
| '-'                          -> true
| c when System.Char.IsDigit c -> true
| _                            -> false

let token =
  rstate {
    let! jc = nextChar
    let f = tokenAtChar jc
    let! r =
      match jc.Char with
      | '{' -> f LeftCurly    |> unit
      | '}' -> f RightCurly   |> unit
      | '[' -> f LeftBracket  |> unit
      | ']' -> f RightBracket |> unit
      | ':' -> f Colon        |> unit
      | ',' -> f Comma        |> unit
      | 't' -> trueToken jc
      | 'f' -> falseToken jc
      | 'n' -> nullToken jc
      | '"' -> stringToken [ ] |> fmap f
      | c when numericLeader c ->
        fmap f (numericToken jc)
      | _   -> unexpectedInput jc |> fail
    return r
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
