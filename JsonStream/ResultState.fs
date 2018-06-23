module ResultState

type ResultState<'a, 'b, 's> =
  ResultState of ('s -> Result<'a, 'b> * 's)

let runState (ResultState fn) s =
  fn s

let unit x =
  ResultState (fun s -> Ok x, s)

(*
  Concrete example:
  'state: LazyList<char>
  'a: char
  'err: ParseError
*)

let bind (x: ResultState<'a, 'err, 'state>) (f: 'a -> ResultState<'b, 'err, 'state>): ResultState<'b, 'err, 'state> =
  let fn s =
    // run the wrapped function with s to get a diminished list and a Result<'a, 'err>
    let result, newState = runState x s
    // if the result is Ok, run f on its contents, producing a new wrapped state function.
    // if not, don't run anything and just return a state function that ignores its input and returns the error
    let wrappedStateFn = Result.map f result
    runState wrappedStateFn newState
  ResultState fn
