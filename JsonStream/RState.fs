module JsonStream.RState

type RState<'s, 'a, 'err> = RState of ('s -> Result<('a * 's), 'err>)
let runStateR (RState f) s = f s

let unit x =
  RState (fun s -> Ok (x, s))

let fail e =
  RState (fun _ -> Error e)

type RStateBuilder() =
  member __.Return(x) = unit x

  member __.Zero() = unit ()

  member __.ReturnFrom(x) = x

  member __.Yield(x) = unit x

  member __.Bind(RState f, x) =
    let fn s =
      let pair = f s
      let binder (v, s2) = runStateR (x v) s2
      Result.bind binder pair
    RState fn

let rstate = new RStateBuilder()

let get =
  RState (fun s -> Ok (s, s))

let put x =
  let fn = fun _ -> Ok ((), x)
  RState fn

let fmap f (RState x) =
  let mapper (content, state) = f content, state
  let fn s = Result.map (fun (content, state) -> (f content, state)) (x s)
  RState fn
