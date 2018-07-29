open Argu
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open System.Reflection
open Microsoft.CodeAnalysis.CSharp.Syntax
open JsonDeserializer.Deserializer
open JsonStream.StateOps
open ClassGenerator.JsonParser
open System.Text.RegularExpressions

type Arguments =
  | [<Mandatory>] Schema of path:string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Schema _ -> "Path to a JSON schema document"

let mkAssembly n (tree: SyntaxNode) =
  // let corlibloc = (typeof<Object>).Assembly.Location
  // let mscorlib = MetadataReference.CreateFromFile(corlibloc)
  let x = SyntaxFactory.CompilationUnit()
  let y = CSharpSyntaxTree.Create(x)
  let syntaxTrees = [ y; ]
  let refs = [  ]
  let opts = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
  let compilation = CSharpCompilation.Create(n, syntaxTrees, refs, opts)
  compilation.Emit(sprintf "%s.dll" n)

let logCompilation (r: Emit.EmitResult) =
  printfn "Compilation success: %b" r.Success
  for d in r.Diagnostics do
    printfn "%A [%s]: %s" d.Severity d.Id (d.GetMessage())

let genType =
  SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.StringKeyword))

let genId n =
  SyntaxFactory.Identifier(n)

let genInterface (n: string) ps =
  let folder (decl: InterfaceDeclarationSyntax) k v =
    let t = genType
    let id = genId k
    let mem = SyntaxFactory.PropertyDeclaration(t, id)
    decl.AddMembers(mem)
  let iface = SyntaxFactory.InterfaceDeclaration(n)
  Map.fold folder iface ps

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

  fromFile schemaFile
  |> Result.bind (readInterface asmName)
  |> Result.map (mkAssembly asmName)
  |> Result.map (fun _ -> printfn "Done!")
  |> Result.mapError (fun e -> printfn "%A" e)
  |> ignore

  // Exit code
  0
