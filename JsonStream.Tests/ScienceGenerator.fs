module JsonStream.Tests.ScienceGenerator

open FsCheck

let sign = Gen.elements [ "+"; "-"; ""; ]

let e = Gen.elements [ "e"; "E"; ]

let num = Arb.generate<uint32>

let float = Arb.generate<NormalFloat>

type Scientific = { Num: string }

let scientific = Gen.map4 (fun num e sign pow ->
  let numStr = num.ToString()
  let powStr = pow.ToString()
  { Num = System.String.Concat [ numStr; e; sign; powStr; ] }) float e sign num

type ScientificGenerators =
  static member ScientificNotation() =
    Arb.fromGen scientific
