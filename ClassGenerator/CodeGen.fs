module ClassGenerator.CodeGen

open JsonDeserializer.Types
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open ClassGenerator.Names
open JsonSchema.Types
open EnvLogger

type SF = SyntaxFactory
type SK = SyntaxKind

let log = new Logger()

let genArrayMember name node =
  let genericName = SF.Identifier("IReadOnlyList") |> SF.GenericName
  let id = SF.Identifier(name)
  let typ =
    SK.IntKeyword // TODO
    |> SF.Token
    |> SF.PredefinedType
    |> SF.SingletonSeparatedList<TypeSyntax>
    |> SF.TypeArgumentList
    |> genericName.WithTypeArgumentList
  SF.PropertyDeclaration(typ, id)

let genObjMember name node =
  // TODO
  genArrayMember name node

let genPreKeyword node =
  match node with
  | JsonNode.Null      -> SK.ObjectKeyword
  | JsonNode.Boolean _ -> SK.BoolKeyword
  | JsonNode.String _  -> SK.StringKeyword
  | JsonNode.Number _  -> SK.DoubleKeyword
  | x         -> sprintf "Unspported builtin %A" x |> failwith
  |> SF.Token
  |> SF.PredefinedType

let genPreMember name node =
  let typ = genPreKeyword node
  let nam = SF.Identifier name
  SF.PropertyDeclaration(typ, nam)

let genMember name node =
  match node with
  | JsonNode.Null
  | JsonNode.Boolean _
  | JsonNode.String _
  | JsonNode.Number _ ->
    genPreMember (memberName name) node
  | JsonNode.Array _ ->
    genArrayMember (memberName name) node
  | JsonNode.Object _ ->
    // genObjMember name node
    genPreMember name JsonNode.Null

let genField (x: PropertyDeclarationSyntax) =
  let accs = SK.GetAccessorDeclaration |> SF.AccessorDeclaration
  let accs = SK.SemicolonToken |> SF.Token |> accs.WithSemicolonToken
  let accs = accs |> SF.SingletonList<AccessorDeclarationSyntax> |> SF.AccessorList
  x.WithAccessorList(accs)

let genInterface (n: string) (schema: JsonSchema) =
  sprintf "Generating interface for %s" n |> log.Info
  let folder (decl: InterfaceDeclarationSyntax) k v =
    let fieldDecl = genMember k v |> genField
    decl.AddMembers(fieldDecl)

  let iface =
    SK.PublicKeyword
    |> SF.Token
    |> SF.TokenList
    |> SF.InterfaceDeclaration(n).WithModifiers

  match schema with
  | ObjectSchema { Assertions = asrt; Annotations = annt; } ->
    let members = List.tryPick (function|Assertion.Properties xs->Some xs|_->None) asrt
    let typ = List.tryPick (function|Assertion.Type(ScalarType x)->Some x|_->None) asrt
    Map.fold folder iface schema
  | TrueSchema
  | FalseSchema ->
    Error "Invalid schema type"

let genCompUnit mbr =
  let name = SF.QualifiedName(SF.QualifiedName(SF.IdentifierName("System"), SF.IdentifierName("Collections")), SF.IdentifierName("Generic"))
  let usings = name |> SF.UsingDirective |> SF.SingletonList<UsingDirectiveSyntax>
  SF.CompilationUnit().WithUsings(usings).WithMembers(SF.SingletonList(mbr))
