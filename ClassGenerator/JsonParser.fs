module ClassGenerator.JsonParser

open JsonDeserializer.Deserializer
open JsonStream.Tokenizer
open System.IO
open System.Text
open FSharpx.Collections

let foldResultFlat f xs =
  let folder (list, e) x =
    match x with
    | Ok x    -> (LazyList.cons x list, e)
    | Error e -> list, Error e

  let l, e =
    LazyList.fold folder (LazyList.empty, Ok ()) xs

  match e with
  | Error e -> Error e
  | _       ->
    match f (LazyList.rev l) with
    | Ok x -> Ok x
    | Error e -> Error e

let fromFile f =
  let fs = new FileStream(f, FileMode.Open)
  let reader = new StreamReader(fs, Encoding.UTF8, false)

  let unfolder (s: StreamReader) =
    match s.EndOfStream with
    | true  -> None
    | false -> Some (s.Read() |> char, s)

  LazyList.unfold unfolder reader
  |> tokenize
  |> foldResultFlat deserialize
