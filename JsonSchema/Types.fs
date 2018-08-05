module rec JsonSchema.Types

open JsonDeserializer.Types

// TODO:
// - ECMA262 regex validation
// - Numeric precision?

type JsonSchema =
  | TrueSchema
  | FalseSchema
  | MapSchema of Map<string, Constraint>

// Validation keywords for any instance type

[<RequireQualifiedAccess>]
type TypeConstraint =
  | Null
  | Boolean
  | Object
  | Array
  | Number
  | String
  | Integer

type EnumConstraint = EnumConstraint of JsonNode[]

type ConstConstraint = ConstConstraint of JsonNode

// Validation keywords for numeric instances

type MultipleOfConstraint = MultipleOf of double

type MaximumConstraint = Maximum of double

type ExclusiveMaximumConstraint = ExclusiveMaximum of double

type MinimumConstraint = Minimum of double

type ExclusiveMinimumConstraint = ExclusiveMinimum of double

// Validation keywords for strings

type MaxLengthConstraint = MaxLengthConstraint of uint64

type MinLengthConstraint = MinLengthConstraint of uint64

type PatternConstraint = PatternConstraint of string

// Validation keywords for arrays

type ItemsConstraint =
  | SingletonItemSchema of JsonSchema
  | MultiJsonSchema of JsonSchema[]

type AdditionalItemsConstraint = AdditionalItemsConstraint of JsonSchema

type MaxItemsConstraint = MaxItemsConstraint of uint64

type MinItemsConstraint = MinItemsConstraint of uint64

type UniqueItemsConstraint = UniqueItemsConstraint of bool

type ContainsConstraint = ContainsConstraint of JsonSchema

// Validation keywords for objects

type MaxPropertiesConstraint = MaxPropertiesConstraint of uint64

type MinPropertiesConstraint = MinPropertiesConstraint of uint64

type RequiredConstraint = RequiredConstraint of Set<string>

type PropertiesConstraint = PropertiesConstraint of Map<string, JsonSchema>

type PatternPropertiesConstraint = PatternPropertiesConstraint of Map<string, JsonSchema>

type AdditionalPropertiesConstraint = AdditionalItemsConstraint of JsonSchema

type Dependency =
  | Schema of JsonSchema
  | Array of Set<string>

type DependenciesConstraint = DependenciesConstraint of Map<string, Dependency>

type PropertyNamesConstraint = PropertyNamesConstraint of JsonSchema

type Constraint =
  | EnumConstraint
  | ConstConstraint
  | MultipleOfConstraint
  | MaximumConstraint
  | ExclusiveMaximumConstraint
  | MinimumConstraint
  | ExclusiveMinimumConstraint
  | MaxLengthConstraint
  | MinLengthConstraint
  | PatternConstraint
  | ItemsConstraint
  | AdditionalItemsConstraint
  | MaxItemsConstraint
  | MinItemsConstraint
  | UniqueItemsConstraint
  | ContainsConstraint
  | MaxPropertiesConstraint
  | MinPropertiesConstraint
  | RequiredConstraint
  | PropertiesConstraint
  | PatternPropertiesConstraint
  | AdditionalPropertiesConstraint
  | DepenedenciesConstraint
  | PropertyNamesConstraint
