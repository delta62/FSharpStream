module rec JsonSchema.Types

open JsonDeserializer.Types

// TODO:
// - ECMA262 regex validation
// - Numeric precision?

type JsonSchema =
  | TrueSchema
  | FalseSchema
  | ObjectSchema of Assertion list * Annotation list

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

type Annotation = Annotation

type ItemsAssertion =
  | SingletonItemSchema of JsonSchema
  | MultiJsonSchema of JsonSchema[]

type Dependency =
  | Schema of JsonSchema
  | Array of Set<string>

[<RequireQualifiedAccess>]
type Assertion =
  // Validation keywords for any instance type
  | Type                 of TypeAssertion
  | Enum                 of JsonNode list
  | Const                of JsonNode
  // Validation keywords for numeric instances
  | MultipleOf           of double
  | Maximum              of double
  | ExclusiveMaximum     of double
  | Minimum              of double
  | ExclusiveMinimum     of double
  // Validation keywords for strings
  | MaxLength            of uint64
  | MinLength            of uint64
  | Pattern              of string
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
  | Required             of Set<string>
  | Properties           of Map<string, JsonSchema>
  | PatternProperties    of Map<string, JsonSchema>
  | AdditionalProperties of JsonSchema
  | Depenedenciesof      of Map<string, Dependency>
  | PropertyNames        of JsonSchema
