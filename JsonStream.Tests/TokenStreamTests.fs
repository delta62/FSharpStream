module JsonStream.Tests.TokenStreamTests

open Expecto
open JsonStream.StateOps
open JsonStream.TokenStream
open FSharpx.Collections

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

[<Tests>]
let tests =
  testList "TokenStream" [
    testCase "Fails on empty input" <| fun _ ->
      let subject = tokenStream LazyList.empty
      Expect.isError (LazyList.head subject) "Successfully parsed empty input"

    testCase "Fails on whitespace input" <| fun _ ->
      let subject =
        [ Whitespace " " ]
        |> List.map toJsonVal
        |> LazyList.ofList
        |> tokenStream
      Expect.isError (LazyList.head subject) "Successfully parsed whitespace input"

    testCase "Successfully parses whitespace padded literal" <| fun _ ->
      let stream =
        [ Whitespace " "; True; Whitespace "\n"; ]
        |> List.map toJsonVal
        |> LazyList.ofList
      let subject = tokenStream stream
      Expect.isOk (LazyList.head subject) "Failed to parse whitespace padded literal"
  ]
