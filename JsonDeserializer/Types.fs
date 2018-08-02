module JsonDeserializer.Types

type JsonNode =
  | Null
  | Boolean of bool
  | String  of string
  | Number  of string
  | Array   of JsonNode list
  | Object  of Map<string, JsonNode>
