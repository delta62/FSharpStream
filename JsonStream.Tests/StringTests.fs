module JsonStream.Tests.StringTests

open Expecto
open FSharpx.Collections
open JsonStream.Tokenizer
open JsonStream.StateOps

let stringToken x =
  let str = x.ToString()
  let list = LazyList.ofSeq str |> tokenize
  match list.TryHead with
    | Some (Ok ({ Token = String x})) -> Some x
    | _ -> None

[<Tests>]
let tests =
  testList "String tokens" [
    testCase "Tokenizes an empty string" <| fun _ ->
      let subject = stringToken "\"\""
      Expect.equal subject (Some "") "Couldn't tokenize an empty string"

    testCase "Tokenizes escape sequences" <| fun _ ->
      let subject = stringToken "\"\\n\""
      Expect.equal subject (Some "\n") "Couldn't tokenize newline escape"

    testCase "Tokenizes unicode escapes" <| fun _ ->
      let subject = stringToken "\"\\u10FC\""
      Expect.equal subject (Some "\u10FC") "Failed to tokenize \u10FC"

    testCase "Tokenizes multi-character unicode escapes" <| fun _ ->
      let subject = stringToken "\"\\u0001\\uF3A9\""
      Expect.equal subject (Some "\U00001F3A9") "Failed to tokenize '\U0001F3A9'"

    ptestCase "Fails on invalid surrogate pairs" <| fun _ ->
      Expect.isTrue false "Successfully tokenized an invalid surrogate pair"
  ]
