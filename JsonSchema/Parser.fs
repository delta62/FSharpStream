module JsonSchema.Parser

open JsonDeserializer.Types

let parse node =
    match node with
    | Object _ ->
        Ok ()
    | _ ->
        Error "Root level items must be objects"
