module ClassGenerator.Program

open Argu
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open System.Reflection
open JsonStream
open JsonDeserializer
open ClassGenerator.JsonParser
open ClassGenerator.CodeGen
open System.Text.RegularExpressions

type Arguments =
  | [<Mandatory>] Schema of path:string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Schema _ -> "Path to a JSON schema document"

let mkAssembly n tree =
  let x = SyntaxFactory.CompilationUnit().WithMembers(SyntaxFactory.SingletonList(tree))
  let y = CSharpSyntaxTree.Create(x)
  let syntaxTrees = [ y; ]
  let refs = [  ]
  let opts = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
  let compilation = CSharpCompilation.Create(n, syntaxTrees, refs, opts).AddReferences(MetadataReference.CreateFromFile(typeof<System.Object>.Assembly.Location))
  compilation.Emit(sprintf "%s.dll" n)

let logCompilation (r: Emit.EmitResult) =
  printfn "Compilation success: %b" r.Success
  for d in r.Diagnostics do
    printfn "%A [%s]: %s" d.Severity d.Id (d.GetMessage())

let readInterface n i =
  match i with
  | JsonNode.Object ps -> genInterface n ps |> Ok
  | _ -> Error { Line = 0u; Column = 0u; Message = "Only top-level objects are supported" }

let asmFromFile f =
  Regex.Replace(f, @"\.json$", "")

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create<Arguments>(programName = "cgen.exe")
  let args = parser.Parse argv

  let schemaFile = args.GetResult Schema
  let asmName = schemaFile |> asmFromFile
  printfn "Compiling %s..." asmName

  let res =
    fromFile schemaFile
    |> Result.bind (readInterface asmName)
    |> Result.map (mkAssembly asmName)

  // Exit code
  match res with
  | Ok x ->
    logCompilation x
    printfn "Done!"
    0
  | Error e ->
    printfn "%s(%d,%d): %s" schemaFile e.Line e.Column e.Message
    printfn "Error while compiling library"
    1
