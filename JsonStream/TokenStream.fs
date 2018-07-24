module JsonStream.TokenStream

open FSharpx.Collections
open JsonStream.StateOps
open System.Runtime.InteropServices.ComTypes

type JsonToken = Result<JsonVal<Token>, ParseError>
type JsonList = LazyList<JsonToken>

type JsonContext =
  | Root
  | Object
  | ObjectKey
  | ObjectColon
  | ObjectValue
  | Array
  | ArrayValue

let unexpectedEof =
  Error {
    Line    = 0u;
    Column  = 0u;
    Message = "Unexpected end of input";
  }

let unexpectedInput input =
 Error {
    Line    = input.Line;
    Column  = input.Column;
    Message = sprintf "Unexpected input: %A" input.Val;
  }

let value token ctx =
  match token.Val with
  | String _
  | Number _
  | True
  | False
  | Null        -> Ok (token, ctx)
  | LeftCurly   -> Ok (token, Object :: ctx)
  | LeftBracket -> Ok (token, Array :: ctx)
  | _           -> unexpectedInput token

let object token ctx =
  match token.Val with
  | String _   -> Ok (token, ObjectKey :: ctx)
  | RightCurly -> Ok (token, List.tail ctx)
  | _          -> unexpectedInput token

let objectVal token ctx =
  match token.Val with
  | RightCurly -> Ok (token, List.skip 2 ctx)
  | Comma      -> Ok (token, List.tail ctx)
  | _          -> unexpectedInput token

let array token ctx =
  match token.Val with
  | RightBracket -> Ok (token, List.tail ctx)
  | _ ->
    value token ctx |> Result.map (fun (t, c) -> t, ArrayValue :: c)

let arrayVal token ctx =
  match token.Val with
  | Comma        -> Ok (token, List.tail ctx)
  | RightBracket -> Ok (token, List.skip 2 ctx)
  | _            -> unexpectedInput token

let objectKey token ctx =
  match token.Val with
  | Colon ->
    let newctx = ObjectColon :: (List.tail ctx)
    Ok (token, newctx)
  | _ ->
    unexpectedInput token

let objectColon token ctx =
  value token ctx |> Result.map (fun (t, c) -> t, List.tail c)

let token tok ctx =
  match tok.Val with
  | Whitespace _ -> Ok (tok, ctx)
  | _ ->
    let f =
      match List.head ctx with
      | Root        -> value
      | Object      -> object
      | ObjectKey   -> objectKey
      | ObjectColon -> objectColon
      | ObjectValue -> objectVal
      | Array       -> array
      | ArrayValue  -> arrayVal
    f tok ctx

let emptyStream list =
  let finder = function
  | Ok ({ Val = Whitespace _ }) -> false
  | Error _                     -> false
  | Ok _                        -> true

  list |> LazyList.tryFind finder |> Option.isNone

let cleanEofOrFail ctx =
  match ctx with
  | [ Root; ] -> None
  | _         -> Some (unexpectedEof, (LazyList.empty, [ ]))

let tokenStream list =
  let unfolder (tokenList, contextList) =
    match LazyList.tryHead tokenList with
    | None ->
      cleanEofOrFail contextList
    | Some head ->
      let result =
        head
        |> Result.bind (fun tok -> token tok contextList)
        |> Result.map (fun (t, ctx) -> t, ((LazyList.tail tokenList), ctx))
      match result with
      | Ok (token, state) -> Some (Ok token, state)
      | Error e           -> Some (Error e, (LazyList.empty, [ ]))

  let mappedList = LazyList.unfold unfolder (list, [ Root ])

  // Empty JSON documents are invalid.
  if emptyStream mappedList then
    LazyList.ofList [ unexpectedEof ]
  else
    mappedList
