namespace JsonStream

open FSharpx.Collections

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

type JsonVal<'a> = {
  Line   : uint32;
  Column : uint32;
  Val    : 'a;
}

type TokenizerState<'a> = {
  LastVal : JsonVal<'a>
  List    : LazyList<JsonVal<'a>>;
}