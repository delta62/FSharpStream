module ClassGenerator.Compiler

open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis
open ClassGenerator.Names

let mkAssembly n cu =
  let y = CSharpSyntaxTree.Create cu
  let syntaxTrees = [ y; ]
  let refs = [  ]
  let opts = new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary)
  let compilation = CSharpCompilation.Create(n, syntaxTrees, refs, opts).AddReferences(MetadataReference.CreateFromFile(typeof<System.Object>.Assembly.Location))
  dllFromAsm n |> compilation.Emit

let logCompilation (r: Emit.EmitResult) =
  printfn "Compilation success: %b" r.Success
  for d in r.Diagnostics do
    printfn "%A [%s]: %s" d.Severity d.Id (d.GetMessage())
