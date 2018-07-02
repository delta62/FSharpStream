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

[<Tests>]
let tests =
  testList "Scalar Tokens" [
    testCase "Tokenizes null" <| fun _ ->
      let subject = LazyList.ofSeq "null"
      let result = tokenize subject
      let isNull x = x = Null
      Expect.isTrue (singletonList isNull result) "failed to tokenize \"null\""

    testCase "Tokenizes true" <| fun _ ->
      let subject = LazyList.ofSeq "true"
      let result = tokenize subject
      let isTrue x = x = True
      Expect.isTrue (singletonList isTrue result) "failed to tokenize \"true\""

    testCase "Tokenizes false" <| fun _ ->
      let subject = LazyList.ofSeq "false"
      let result = tokenize subject
      let isFalse x = x = False
      Expect.isTrue (singletonList isFalse result) "failed to tokenize \"false\""

    testCase "Tokenizes {" <| fun _ ->
      let subject = LazyList.ofSeq "{"
      let result = tokenize subject
      let isLeftCurly x = x = LeftCurly
      Expect.isTrue (singletonList isLeftCurly result) "failed to tokenize \"{\""

    testCase "Tokenizes }" <| fun _ ->
      let subject = LazyList.ofSeq "}"
      let result = tokenize subject
      let isRightCurly x = x = RightCurly
      Expect.isTrue (singletonList isRightCurly result) "failed to tokenize \"}\""

    testCase "Tokenizes [" <| fun _ ->
      let subject = LazyList.ofSeq "["
      let result = tokenize subject
      let isLeftBracket x = x = LeftBracket
      Expect.isTrue (singletonList isLeftBracket result) "failed to tokenize \"[\""

    testCase "Tokenizes ]" <| fun _ ->
      let subject = LazyList.ofSeq "]"
      let result = tokenize subject
      let isRightBracket x = x = RightBracket
      Expect.isTrue (singletonList isRightBracket result) "failed to tokenize \"]\""

    testCase "Tokenizes :" <| fun _ ->
      let subject = LazyList.ofSeq ":"
      let result = tokenize subject
      let isColon x = x = Colon
      Expect.isTrue (singletonList isColon result) "failed to tokenize \":\""

    testCase "Tokenizes ," <| fun _ ->
      let subject = LazyList.ofSeq ","
      let result = tokenize subject
      let isComma x = x = Comma
      Expect.isTrue (singletonList isComma result) "failed to tokenize \",\""
  ]
