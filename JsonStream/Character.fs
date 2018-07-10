module JsonStream.Character

let fromHex = function
| '0' -> 0
| '1' -> 1
| '2' -> 2
| '3' -> 3
| '4' -> 4
| '5' -> 5
| '6' -> 6
| '7' -> 7
| '8' -> 8
| '9' -> 9
| 'a' -> 10
| 'A' -> 10
| 'b' -> 11
| 'B' -> 11
| 'c' -> 12
| 'C' -> 12
| 'd' -> 13
| 'D' -> 13
| 'e' -> 14
| 'E' -> 14
| 'f' -> 15
| 'F' -> 15
| _   -> 0

let hexChar = function
| c when System.Char.IsDigit c -> true
| c when c >= 'a' && c <= 'f'  -> true
| c when c >= 'A' && c <= 'F'  -> true
| _                            -> false

let unescaped (x: char) =
  match uint32 x with
  | c when c >= 0x20u && c <= 0x21u     -> true
  | c when c >= 0x23u && c <= 0x5Bu     -> true
  | c when c >= 0x5Du && c <= 0x10FFFFu -> true
  | _                                   -> false

let isSurrogateChar c =
  c >= '\uD800' && c <= '\uDFFF'
