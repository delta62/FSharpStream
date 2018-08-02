module JsonStream.Tests.NumberTests

open Expecto
open FSharpx.Collections
open JsonStream.Tokenizer
open JsonStream.Types
open FsCheck
open ScienceGenerator

let singleNumber x =
  let str = x.ToString()
  let list = LazyList.ofSeq str |> tokenize
  match list.TryHead with
    | Some (Ok ({ Val = Token.Number x})) -> x = str
    | _ -> false

let cfg =
  { FsCheckConfig.defaultConfig with
      arbitrary = [typeof<ScientificGenerators>]
  }

[<Tests>]
let properties =
  testList "Number properties" [
    testProperty "floats can be tokenized" <| fun (x: NormalFloat) -> singleNumber x
    testProperty "ints can be tokenized" <| fun (x: int) -> singleNumber x
    testPropertyWithConfig
      cfg
      "scientific notation can be tokenized"
      (fun (x: Scientific) -> singleNumber x.Num)
  ]

[<Tests>]
let tests =
  testList "Number tests" [
    testCase "Tokenizes really huge numbers" <| (fun _ ->
      let digits = List.replicate 500 '9'
      let subject = Array.ofList digits |> System.String
      Expect.isTrue (singleNumber subject) "Failed to tokenize huge number"
    )
  ]
