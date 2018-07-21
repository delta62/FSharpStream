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

let rec streamEquals actual expected =
  match actual, expected with
  | LazyList.Nil, [ ] -> true
  | LazyList.Cons(Ok x,xs), [ y ] ->
    x.Val = y && streamEquals xs [ ]
  | LazyList.Cons(Ok x,xs), y :: ys ->
    x.Val = y && streamEquals xs ys
  | _ -> false

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
      let subject = "{ }" |> LazyList.ofSeq |> tokenize |> tokenStream
      let expected = [ LeftCurly; Whitespace " "; RightCurly; ]
      Expect.isTrue (streamEquals subject expected) "Failed to parse an empty object"
  ]
