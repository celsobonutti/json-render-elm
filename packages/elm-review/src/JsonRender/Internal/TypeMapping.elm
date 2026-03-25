module JsonRender.Internal.TypeMapping exposing
    ( capitalizeFirst
    , enumTypeDeclaration
    , toElmType
    , toJsonDecoder
    , toResolvedValueExtractor
    )

import JsonRender.Internal.SchemaParser exposing (FieldType(..))


toElmType : FieldType -> String
toElmType fieldType =
    case fieldType of
        FString ->
            "String"

        FInt ->
            "Int"

        FFloat ->
            "Float"

        FBool ->
            "Bool"

        FNullable inner ->
            "Maybe " ++ toElmType inner

        FList inner ->
            "List " ++ toElmType inner

        FEnum variants ->
            capitalizeFirst (String.join "Or" (List.map capitalizeFirst variants))

        FObject _ ->
            "Value"


toJsonDecoder : FieldType -> String
toJsonDecoder fieldType =
    case fieldType of
        FString ->
            "Decode.string"

        FInt ->
            "Decode.int"

        FFloat ->
            "Decode.float"

        FBool ->
            "Decode.bool"

        FNullable inner ->
            "(Decode.nullable " ++ toJsonDecoder inner ++ ")"

        FList inner ->
            "(Decode.list " ++ toJsonDecoder inner ++ ")"

        FEnum _ ->
            "Decode.string"

        FObject _ ->
            "Decode.value"


toResolvedValueExtractor : FieldType -> String
toResolvedValueExtractor fieldType =
    case fieldType of
        FString ->
            "ResolvedValue.string"

        FInt ->
            "ResolvedValue.int"

        FFloat ->
            "ResolvedValue.float"

        FBool ->
            "ResolvedValue.bool"

        _ ->
            "ResolvedValue.string"


enumTypeDeclaration : String -> List String -> String
enumTypeDeclaration typeName variants =
    let
        variantStrings =
            List.map capitalizeFirst variants

        body =
            case variantStrings of
                [] ->
                    ""

                first :: rest ->
                    "    = "
                        ++ first
                        ++ String.concat (List.map (\v -> "\n    | " ++ v) rest)
    in
    "type " ++ typeName ++ "\n" ++ body


capitalizeFirst : String -> String
capitalizeFirst s =
    case String.uncons s of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            s
