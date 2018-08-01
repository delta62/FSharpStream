module ClassGenerator.CodeGen

open JsonDeserializer
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

type SF = SyntaxFactory
type SK = SyntaxKind

let genArrayMember name node =
  let genericName = SF.Identifier("ImmutableArray") |> SF.GenericName
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
  | Null      -> SK.ObjectKeyword
  | Boolean _ -> SK.BoolKeyword
  | String _  -> SK.StringKeyword
  | Number _  -> SK.DoubleKeyword
  | x         -> sprintf "Unspported builtin %A" x |> failwith
  |> SF.Token
  |> SF.PredefinedType

let genPreMember name node =
  let typ = genPreKeyword node
  let nam = SF.Identifier name
  SF.PropertyDeclaration(typ, nam)

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

let genField (x: PropertyDeclarationSyntax) =
  let accs = SK.GetAccessorDeclaration |> SF.AccessorDeclaration
  let accs = SK.SemicolonToken |> SF.Token |> accs.WithSemicolonToken
  let accs = accs |> SF.SingletonList<AccessorDeclarationSyntax> |> SF.AccessorList
  x.WithAccessorList(accs)

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
