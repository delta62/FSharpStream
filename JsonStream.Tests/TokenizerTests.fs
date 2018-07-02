module Tests

open Expecto
open FSharpx.Collections
open Tokenizer

let singletonList f xs =
  match LazyList.length xs with
  | 1 ->
    match LazyList.head xs with
    | Ok x when f x.Token -> true
    | _ -> false
  | _ -> false

let makeTest (input, expected) =
  testCase (sprintf "Tokenizes %s" input) <| fun _ ->
    let result = LazyList.ofSeq input |> tokenize
    let isOk x = x = expected
    Expect.isTrue (singletonList isOk result) (sprintf "Failed to tokenize \"%s\"" input)

let inputs = [
  ("null", Null)
  ("true", True)
  ("false", False)
  ("{", LeftCurly)
  ("}", RightCurly)
  ("[", LeftBracket)
  ("]", RightBracket)
  (":", Colon)
  (",", Comma)
]

[<Tests>]
let tests =
  testList "Scalar Tokens" <| List.map makeTest inputs
