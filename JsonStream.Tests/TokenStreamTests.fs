module JsonStream.Tests.TokenStreamTests

open Expecto
open JsonStream.TokenStream
open FSharpx.Collections

[<Tests>]
let tests =
  testList "TokenStream" [
    testCase "Fails on empty input" <| fun _ ->
      let subject = tokenStream LazyList.empty
      Expect.isError (LazyList.head subject) "Successfully parsed empty input"
  ]
