module JsonStream.Tests.StringTests

open Expecto
open FSharpx.Collections
open JsonStream.Tokenizer
open JsonStream.StateOps

let stringToken x =
  let list = LazyList.ofSeq x |> tokenize
  match list.TryHead with
    | Some (Ok ({ Token = String x})) -> Some x
    | _ -> None

let failString x =
  let list = LazyList.ofSeq x |> tokenize
  match list.TryHead with
  | Some (Error e) -> Error e
  | _              -> Ok ()

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
      let subject = stringToken "\"\\uD83D\\uDE0E\""
      Expect.equal subject (Some "\U0001F60E") "Failed to tokenize '\U0001F60E'"

    testCase "Fails on invalid surrogate pairs" <| fun _ ->
      let subject = failString "\"\\uD83D\\u0032\""
      Expect.isError subject "Successfully tokenized an invalid surrogate pair"

    testCase "Fails to tokenize reserved points" <| fun _ ->
      let subject = failString "\"\\uD83C\""
      Expect.isError subject "Successfully tokenized reserved char"
  ]
