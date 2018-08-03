module ClassGenerator.Names

open System
open System.Text.RegularExpressions

let asmFromFile f =
  Regex.Replace(f, @"\.json$", "")

let dllFromAsm a =
  sprintf "%s.dll" a

let memberName (n: string) =
  match n.Length with
  | 0 -> n
  | _ -> sprintf "%c%s" (n.Chars 0) (n.Substring 1)
