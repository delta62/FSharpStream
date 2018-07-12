module JsonStream.State

type State<'s, 'a> = State of ('s -> 'a * 's)

let runState s (State f) = f s

type StateBuilder() =
  member __.Return(x) =
    State (fun s -> x, s)

  member __.Bind(State mx, f) =
    let fn s =
      let a, newState = mx s
      let (State b) = f a
      b newState
    State fn

let state = new StateBuilder()

let get =
  State (fun s -> (s, s))

let set x =
  State (fun _ -> ((), x))
