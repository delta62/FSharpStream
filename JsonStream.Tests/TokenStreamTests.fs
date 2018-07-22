module JsonStream.Tests.TokenStreamTests

open Expecto
open JsonStream.StateOps
open JsonStream.TokenStream
open FSharpx.Collections
open JsonStream.Tokenizer

let toJsonVal x = {
  Line = 0u;
  Column = 0u;
  Val = x;
}

let tokenVal list =
  match list with
  | LazyList.Nil -> None
  | LazyList.Cons(h, _) ->
    match h with
    | Ok x -> Some x.Val
    | _ -> None

let toStream str =
  str |> LazyList.ofSeq |> tokenize |> tokenStream

let rec streamEquals actual expected =
  match actual, expected with
  | LazyList.Nil, [ ] -> true
  | LazyList.Cons(Ok x,xs), [ y ] ->
    x.Val = y && streamEquals xs [ ]
  | LazyList.Cons(Ok x,xs), y :: ys ->
    x.Val = y && streamEquals xs ys
  | _ -> false

let scalarInputs = [
  ("null",    Null);
  ("true",    True);
  ("false",   False);
  ("3.14159", Number "3.14159");
  ("\"foo\"", String "foo");
]

[<Tests>]
let tests =
  testList "TokenStream" [
    testCase "Fails on empty input" <| fun _ ->
      let subject = tokenStream LazyList.empty |> LazyList.head
      Expect.isError subject "Successfully parsed empty input"

    testCase "Fails on whitespace input" <| fun _ ->
      let subject =
        [ Whitespace " " ]
        |> List.map toJsonVal
        |> LazyList.ofList
        |> LazyList.map Ok
        |> tokenStream
        |> LazyList.head
      Expect.isError subject "Successfully parsed whitespace input"

    testCase "Successfully parses whitespace padded literal" <| fun _ ->
      let subject =
        [ Whitespace " "; True; Whitespace "\n"; ]
        |> List.map toJsonVal
        |> LazyList.ofList
        |> LazyList.map Ok
        |> tokenStream
        |> LazyList.head
      Expect.isOk subject "Failed to parse whitespace padded literal"

    testCase "Parses an empty object literal" <| fun _ ->
      let subject = "{ }" |> toStream
      let expected = [ LeftCurly; Whitespace " "; RightCurly; ]
      Expect.isTrue (streamEquals subject expected) "Failed to parse an empty object"

    testCase "Parses an empty array literal" <| fun _ ->
      let subject = "[ ]" |> toStream
      let expected = [ LeftBracket; Whitespace " "; RightBracket; ]
      Expect.isTrue (streamEquals subject expected) "Failed to parse an empty array"

    testCase "Parses an object with values" <| fun _ ->
      let subject = "{\"foo\": \"bar\"}" |> toStream
      let expected = [ LeftCurly; String "foo"; Colon; Whitespace " "; String "bar"; RightCurly; ]
      Expect.isTrue (streamEquals subject expected) "Failed to parse object with keys"

    testCase "Parses an array with values" <| fun _ ->
      let subject = "[ 1, \"foo\" ]" |> toStream
      let expected = [ LeftBracket; Whitespace " "; Number "1"; Comma; Whitespace " "; String "foo"; Whitespace " "; RightBracket; ]
      Expect.isTrue (streamEquals subject expected) "Failed to parse array with values"

    testList "Parses top-level scalar inputs" (List.map (fun (input, output) ->
      testCase (sprintf "Parses literal \"%s\"" input) <| fun _ ->
        let subject = input |> toStream
        let expected = [ output; ]
        Expect.isTrue (streamEquals subject expected) "Failed to parse scalar token") scalarInputs)

    testCase "Fails to parse unclosed contexts" <| fun _ ->
      let subject = "[1" |> toStream
      Expect.isOk (LazyList.head subject) "Failed to parse top level array"
      Expect.isOk (subject |> LazyList.skip 1 |> LazyList.head) "Failed to parse array value"
      Expect.isError (subject |> LazyList.skip 3 |> LazyList.head) "Allowed unclosed array"

    testCase "Fails to parse unclosed objects" <| fun _ ->
      let subject = "{\"foo\":\"bar\"" |> toStream
      Expect.isOk (LazyList.head subject) "Failed to parse top level object"
      Expect.isOk (subject |> LazyList.skip 1 |> LazyList.head) "Failed to parse object key"
      Expect.isOk (subject |> LazyList.skip 2 |> LazyList.head) "Failed to parse object colon"
      Expect.isOk (subject |> LazyList.skip 3 |> LazyList.head) "Failed to parse object value"
      Expect.isError (subject |> LazyList.skip 4 |> LazyList.head) "Allowed unclosed object"
  ]
