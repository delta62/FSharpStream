module JsonStream.LazyList

open FSharpx.Collections

let rec takeWhile f xs =
  match xs with
  | LazyList.Nil -> [ ]
  | LazyList.Cons(h, t) ->
    if f h then h :: takeWhile f t else [ ]
