module State

type State<'a, 's> = State of ('s -> 'a * 's)

let runState (State f) s = f s

let unit x =
  State(fun s -> (x, s))

let bind x f =
  let fn s =
    let content, newState = runState x s
    let newerState = f content
    runState newerState newState
  State fn

type StateBuilder() =
  member __.Return x =
    unit x

  member __.Bind(x, f) =
    bind x f

let state = new StateBuilder()

let map f xM =
  state {
    let! x = xM
    return f x
  }

let apply fM xM =
  state {
    let! f = fM
    let! x = xM
    return f x
  }

let get =
  State (fun s -> (s, s))

let put s =
  State (fun _ -> ((), s))
