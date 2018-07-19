module JsonStream.Tests.WhitespaceTests

open Expecto
open FSharpx.Collections
open JsonStream.Tokenizer
open JsonStream.StateOps

let whitespaceToken x =
  let list = LazyList.ofSeq x |> tokenize
  match list.TryHead with
    | Some (Ok ({ Val = Whitespace x})) -> Some x
    | _ -> None

[<Tests>]
let tests =
  testList "Whitespace tokenization" [
    testCase "Tokenizes a single whitespace character" <| fun _ ->
      let subject = whitespaceToken "\n"
      Expect.equal subject (Some "\n") "Failed to tokenize '\\n'"

    testCase "Tokenizes sequential whitespace characters" <| fun _ ->
      let str = "      \t  \r  \n   "
      let subject = whitespaceToken str
      Expect.equal subject (Some str) "Failed to tokenize sequential whitespace"
  ]
