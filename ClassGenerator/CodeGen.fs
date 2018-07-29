module ClassGenerator.CodeGen
open JsonDeserializer.Deserializer
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

type SF = SyntaxFactory
type SK = SyntaxKind

let genArrayMember name node =
  let genericName = SF.Identifier("ImmutableArray") |> SF.GenericName
  let var =
    SK.IntKeyword // TODO
    |> SF.Token
    |> SF.PredefinedType
    |> SF.SingletonSeparatedList<TypeSyntax>
    |> SF.TypeArgumentList
    |> genericName.WithTypeArgumentList
    |> SF.VariableDeclaration

  name
  |> SF.Identifier
  |> SF.VariableDeclarator
  |> SF.SingletonSeparatedList<VariableDeclaratorSyntax>
  |> var.WithVariables

let genObjMember name node =
  // TODO
  genArrayMember name node

let genPreKeyword node =
  match node with
  | Null      -> SK.ObjectKeyword
  | Boolean _ -> SK.BoolKeyword
  | String _  -> SK.StringKeyword
  | Number _  -> SK.DoubleKeyword
  | x         -> sprintf "Unspported builtin %A" x |> failwith
  |> SF.Token
  |> SF.PredefinedType

let genPreMember name node =
  let var = genPreKeyword node |> SF.VariableDeclaration
  let vars = SF.SingletonSeparatedList<VariableDeclaratorSyntax>(SF.VariableDeclarator(SF.Identifier(name)))
  var.WithVariables(vars)

let genMember name node =
  match node with
  | Null
  | Boolean _
  | String _
  | Number _ ->
    genPreMember name node
  | Array _ ->
    genArrayMember name node
  | Object _ ->
    // genObjMember name node
    genPreMember name Null

let genField x =
  SF.FieldDeclaration(x).WithModifiers(SF.TokenList([
    SF.Token(SyntaxKind.PublicKeyword);
    SF.Token(SyntaxKind.ReadOnlyKeyword);
  ]))

let genInterface (n: string) ps =
  let folder (decl: InterfaceDeclarationSyntax) k v =
    let fieldDecl = genMember k v |> genField
    decl.AddMembers(fieldDecl)

  let iface =
    SK.PublicKeyword
    |> SF.Token
    |> SF.TokenList
    |> SF.InterfaceDeclaration(n).WithModifiers

  Map.fold folder iface ps
