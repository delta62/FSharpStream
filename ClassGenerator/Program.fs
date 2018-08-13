module ClassGenerator.Program

open Argu
open JsonStream.Types
open JsonDeserializer.Types
open JsonParser
open CodeGen
open Names
open Compiler
open EnvLogger
open System.IO
open JsonSchema.Parser

let log = new Logger()

type Arguments =
  | [<Mandatory>] Schema of path:string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Schema _ -> "Path to a JSON schema document"

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<Arguments>(programName = "cgen.exe")
  let args = parser.Parse argv

  let schemaFile = args.GetResult Schema
  let asmName = schemaFile |> Path.GetFileName |> asmFromFile

  sprintf "Generating assembly \"%s\"..." asmName |> log.Info

  let res =
    fromFile schemaFile
    |> Result.bind parse
    |> Result.bind (genInterface asmName)
    |> Result.map (genCompUnit)
    |> Result.map (mkAssembly asmName)

  // Exit code
  match res with
  | Ok x ->
    logCompilation x
    log.Info "Done!"
    0
  | Error e ->
    sprintf "%s(%d,%d): %s" schemaFile e.Line e.Column e.Message |> log.Error
    log.Error "Error while compiling library"
    1
