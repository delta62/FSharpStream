module JsonStream.Numbers

open RState
open StateOps

let rec digits acc =
  rstate {
    let! jc = maybeNextChar (fun jc -> System.Char.IsDigit jc.Val)
    match jc with
    | Some c -> return! digits (c.Val :: acc)
    | None   -> return List.rev acc
  }

let zeroOrDigits =
  rstate {
    let! next = expect System.Char.IsDigit
    match next.Val with
    | c when c = '0' -> return [ c ]
    | c -> return! digits [ c ]
  }

let frac =
  rstate {
    let! point = maybeNextChar (fun c -> c.Val = '.')
    match point with
    | Some _ ->
      let! x = expect System.Char.IsDigit
      let! xs = takeWhile (fun jc -> System.Char.IsDigit jc.Val)
      return List.concat [ [ '.' ]; [ x.Val ]; List.map (fun x -> x.Val) xs ]
    | None -> return [ ]
  }

let exp =
  rstate {
    let! e = maybeNextChar (fun c -> c.Val = 'e' || c.Val = 'E')
    match e with
    | Some jc ->
      let! signOpt = maybeNextChar (fun c -> c.Val = '+' || c.Val = '-')
      let sign =
        match signOpt with
        | Some jc -> [ jc.Val ]
        | None -> [ ]
      let! x = expect System.Char.IsDigit
      let! xs = takeWhile (fun jc -> System.Char.IsDigit jc.Val)
      return List.concat [ [ jc.Val ]; sign; [ x.Val ]; List.map (fun x -> x.Val) xs ]
    | None -> return [ ]
  }

let sign =
  rstate {
    let! sign = maybeNextChar (fun c -> c.Val = '-')
    match sign with
    | Some x -> return [ x.Val ]
    | None   -> return [ ]
  }

let numericToken leader =
  rstate {
    do! putChar leader
    let! sign = sign
    let! digits = zeroOrDigits
    let! frac = frac
    let! exp = exp

    return List.concat [ sign; digits; frac; exp; ]
      |> Array.ofList
      |> System.String
      |> Number
  }

let numericLeader = function
| '-'                          -> true
| c when System.Char.IsDigit c -> true
| _                            -> false
