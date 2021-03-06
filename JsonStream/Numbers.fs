module JsonStream.Numbers

open JsonStream.RState
open JsonStream.StateOps
open JsonStream.Types

let rec digits acc =
  rstate {
    let! jc = maybeNextChar <| fun jc -> System.Char.IsDigit jc.Val
    match jc with
    | Some c -> return! digits (c.Val :: acc)
    | None   -> return List.rev acc
  }

let zeroOrDigits leader =
  rstate {
    let l =
      match leader.Val with
      | '-' -> [ leader.Val; ]
      | _   -> [ ]
    let! n =
      match leader.Val with
      | '-' -> expectWith System.Char.IsDigit
      | _ -> unit leader

    match n.Val with
    | '0' ->
      return List.concat [ l; [ n.Val ]; ]
    | c   ->
      let! digits = digits [ c; ]
      return List.concat [ l; digits; ]
  }

let frac =
  rstate {
    let! point = maybeNextChar <| fun c -> c.Val = '.'
    match point with
    | Some _ ->
      let! x = expectWith System.Char.IsDigit
      let! xs = takeWhile <| fun jc -> System.Char.IsDigit jc.Val
      return List.concat [ [ '.' ]; [ x.Val ]; List.map (fun x -> x.Val) xs ]
    | None -> return [ ]
  }

let exp =
  rstate {
    let! e = maybeNextChar <| fun c -> c.Val = 'e' || c.Val = 'E'
    match e with
    | Some jc ->
      let! signOpt = maybeNextChar <| fun c -> c.Val = '+' || c.Val = '-'
      let sign =
        match signOpt with
        | Some jc -> [ jc.Val ]
        | None -> [ ]
      let! x = expectWith System.Char.IsDigit
      let! xs = takeWhile (fun jc -> System.Char.IsDigit jc.Val)
      return List.concat [ [ jc.Val ]; sign; [ x.Val ]; List.map (fun x -> x.Val) xs ]
    | None -> return [ ]
  }

let sign =
  rstate {
    let! sign = maybeNextChar <| fun c -> c.Val = '-'
    match sign with
    | Some x -> return [ x.Val ]
    | None   -> return [ ]
  }

let numericToken leader =
  rstate {
    let! digits = zeroOrDigits leader
    let! frac = frac
    let! exp = exp

    return List.concat [ digits; frac; exp; ]
      |> Array.ofList
      |> System.String
      |> Token.Number
  }

let numericLeader = function
| '-'                          -> true
| c when System.Char.IsDigit c -> true
| _                            -> false
