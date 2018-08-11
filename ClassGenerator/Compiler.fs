module ClassGenerator.Compiler

open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis
open ClassGenerator.Names
open EnvLogger

let log = new Logger()

let mkAssembly n cu =
  let y = CSharpSyntaxTree.Create cu
  let syntaxTrees = [ y; ]
  let refs = [  ]
  let opts = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
  let compilation = CSharpCompilation.Create(n, syntaxTrees, refs, opts).AddReferences(MetadataReference.CreateFromFile(typeof<System.Object>.Assembly.Location))
  dllFromAsm n |> compilation.Emit

let logCompilation (r: Emit.EmitResult) =
  if r.Success then
    sprintf "Compilation successful." |> log.Info
  else
    sprintf "Compilation failed." |> log.Error
  for d in r.Diagnostics do
    match d.Severity with
    | DiagnosticSeverity.Error ->
      sprintf "[%s]: %s" d.Id (d.GetMessage()) |> log.Error
    | DiagnosticSeverity.Warning ->
      sprintf "[%s]: %s" d.Id (d.GetMessage()) |> log.Warn
    | DiagnosticSeverity.Info ->
      sprintf "[%s]: %s" d.Id (d.GetMessage()) |> log.Info
    | DiagnosticSeverity.Hidden ->
      sprintf "[%s]: %s" d.Id (d.GetMessage()) |> log.Trace
    | _ -> ()
