module rec JsonSchema.Types

open JsonDeserializer.Types

// TODO:
// - ECMA262 regex validation
// - Numeric precision?

type JsonSchema =
  | TrueSchema
  | FalseSchema
  | ObjectSchema of Assertion list * Annotation list

// Validation keywords for any instance type

[<RequireQualifiedAccess>]
type ScalarTypeAssertion =
  | Null
  | Boolean
  | Object
  | Array
  | Number
  | String
  | Integer

type TypeAssertion =
  | ScalarType of ScalarTypeAssertion
  | ListType of ScalarTypeAssertion list

type Annotation = Annotation

type EnumAssertion = EnumAssertion of JsonNode list

type ConstAssertion = ConstAssertion of JsonNode

// Validation keywords for numeric instances

type MultipleOfAssertion = MultipleOf of double

type MaximumAssertion = Maximum of double

type ExclusiveMaximumAssertion = ExclusiveMaximum of double

type MinimumAssertion = Minimum of double

type ExclusiveMinimumAssertion = ExclusiveMinimum of double

// Validation keywords for strings

type MaxLengthAssertion = MaxLengthAssertion of uint64

type MinLengthAssertion = MinLengthAssertion of uint64

type PatternAssertion = PatternAssertion of string

// Validation keywords for arrays

type ItemsAssertion =
  | SingletonItemSchema of JsonSchema
  | MultiJsonSchema of JsonSchema[]

type AdditionalItemsAssertion = AdditionalItemsAssertion of JsonSchema

type MaxItemsAssertion = MaxItemsAssertion of uint64

type MinItemsAssertion = MinItemsAssertion of uint64

type UniqueItemsAssertion = UniqueItemsAssertion of bool

type ContainsAssertion = ContainsAssertion of JsonSchema

// Validation keywords for objects

type MaxPropertiesAssertion = MaxPropertiesAssertion of uint64

type MinPropertiesAssertion = MinPropertiesAssertion of uint64

type RequiredAssertion = RequiredAssertion of Set<string>

type PropertiesAssertion = PropertiesAssertion of Map<string, JsonSchema>

type PatternPropertiesAssertion = PatternPropertiesAssertion of Map<string, JsonSchema>

type AdditionalPropertiesAssertion = AdditionalItemsAssertion of JsonSchema

type Dependency =
  | Schema of JsonSchema
  | Array of Set<string>

type DependenciesAssertion = DependenciesAssertion of Map<string, Dependency>

type PropertyNamesAssertion = PropertyNamesAssertion of JsonSchema

type Assertion =
  | EnumAssertion of EnumAssertion
  | ConstAssertion
  | MultipleOfAssertion
  | MaximumAssertion
  | ExclusiveMaximumAssertion
  | MinimumAssertion
  | ExclusiveMinimumAssertion
  | MaxLengthAssertion
  | MinLengthAssertion
  | PatternAssertion
  | ItemsAssertion
  | AdditionalItemsAssertion
  | MaxItemsAssertion
  | MinItemsAssertion
  | UniqueItemsAssertion
  | ContainsAssertion
  | MaxPropertiesAssertion
  | MinPropertiesAssertion
  | RequiredAssertion
  | PropertiesAssertion
  | PatternPropertiesAssertion
  | AdditionalPropertiesAssertion
  | DepenedenciesAssertion
  | PropertyNamesAssertion
