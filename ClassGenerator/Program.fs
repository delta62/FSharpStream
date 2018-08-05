module ClassGenerator.Program

open Argu
open JsonStream.Types
open JsonDeserializer.Types
open ClassGenerator.JsonParser
open ClassGenerator.CodeGen
open ClassGenerator.Names
open ClassGenerator.Compiler
open EnvLogger
open System.IO

let log = new Logger()

type Arguments =
  | [<Mandatory>] Schema of path:string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Schema _ -> "Path to a JSON schema document"

let readInterface n i =
  match i with
  | JsonNode.Object ps -> genInterface n ps |> Ok
  | _ -> Error { Line = 0u; Column = 0u; Message = "Only top-level objects are supported" }

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<Arguments>(programName = "cgen.exe")
  let args = parser.Parse argv

  let schemaFile = args.GetResult Schema
  let asmName = schemaFile |> Path.GetFileName |> asmFromFile

  sprintf "Generating assembly \"%s\"..." asmName |> log.Info

  let res =
    fromFile schemaFile
    |> Result.bind (readInterface asmName)
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
