module JsonStream.TokenStream

open FSharpx.Collections
open JsonStream.StateOps

let tokenStream tokens =
  match tokens with
  | LazyList.Nil -> LazyList.ofArray [| Error { Line = 1u; Column = 1u; Message = "Input was empty"; } |]
  | LazyList.Cons(h, t) ->
    LazyList.empty

