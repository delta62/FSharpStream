module rec JsonSchema.Types

open JsonDeserializer.Types
open System.Text.RegularExpressions
open System.Collections.Generic

type SchemaObject = {
  Assertions  : Assertion list;
  Annotations : Annotation list;
  Conditions  : Condition list;
}

type JsonSchema =
  | TrueSchema
  | FalseSchema
  | ObjectSchema of SchemaObject

[<RequireQualifiedAccess>]
type ScalarType =
  | Null
  | Boolean
  | Object
  | Array
  | Number
  | String
  | Integer

type TypeAssertion =
  | ScalarType of ScalarType
  | ListType of ScalarType list

[<RequireQualifiedAccess>]
type SchemaNumber =
  | Integer of int64
  | Double of double

type ItemsAssertion =
  | SingletonItemSchema of JsonSchema
  | MultiJsonSchema of JsonSchema list

type Dependency =
  | SchemaDependency of JsonSchema
  | ArrayDependency of string list

[<RequireQualifiedAccess>]
type Assertion =
  // Validation keywords for any instance type
  | Type                 of TypeAssertion
  | Enum                 of JsonNode list
  | Const                of JsonNode
  // Validation keywords for numeric instances
  | MultipleOf           of SchemaNumber
  | Maximum              of SchemaNumber
  | ExclusiveMaximum     of SchemaNumber
  | Minimum              of SchemaNumber
  | ExclusiveMinimum     of SchemaNumber
  // Validation keywords for strings
  | MaxLength            of uint64
  | MinLength            of uint64
  | Pattern              of Regex
  // Validation keywords for arrays
  | Items                of ItemsAssertion
  | AdditionalItems      of JsonSchema
  | MaxItems             of uint64
  | MinItems             of uint64
  | UniqueItems          of bool
  | Contains             of JsonSchema
  // Validation keywords for objects
  | MaxProperties        of uint64
  | MinProperties        of uint64
  | Required             of string list
  | Properties           of Map<string, JsonSchema>
  | PatternProperties    of Dictionary<Regex, JsonSchema>
  | AdditionalProperties of JsonSchema
  | Depenedenciesof      of Map<string, Dependency>
  | PropertyNames        of JsonSchema

[<RequireQualifiedAccess>]
type Annotation =
  | Title       of string
  | Description of string
  | Default     of JsonNode
  | ReadOnly    of bool
  | WriteOnly   of bool
  | Examples    of JsonNode list

[<RequireQualifiedAccess>]
type Condition =
  | If    of JsonSchema
  | Then  of JsonSchema
  | Else  of JsonSchema
  | AllOf of JsonSchema list
  | AnyOf of JsonSchema list
  | OneOf of JsonSchema list
  | Not   of JsonSchema
