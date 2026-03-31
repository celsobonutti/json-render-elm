module JsonRender.Internal.TypeMapping exposing
    ( capitalizeFirst
    , enumDecoderFunction
    , enumFnBaseName
    , enumFromStringFunction
    , enumToStringFunction
    , enumTypeDeclaration
    , toElmType
    , toJsonDecoder
    , toResolvedValueExtractor
    , toResolvedValueWrapper
    , toValueEncoder
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

        FEnum variants ->
            enumFnBaseName variants ++ "Decoder"

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

        FEnum variants ->
            "(\\rv -> ResolvedValue.string rv |> Result.andThen " ++ enumFnBaseName variants ++ "FromString)"

        _ ->
            "ResolvedValue.string"


toResolvedValueWrapper : FieldType -> String
toResolvedValueWrapper fieldType =
    case fieldType of
        FString ->
            "RString"

        FInt ->
            "RInt"

        FFloat ->
            "RFloat"

        FBool ->
            "RBool"

        _ ->
            "RString"


toValueEncoder : FieldType -> String
toValueEncoder fieldType =
    case fieldType of
        FString ->
            "Json.Encode.string"

        FInt ->
            "Json.Encode.int"

        FFloat ->
            "Json.Encode.float"

        FBool ->
            "Json.Encode.bool"

        FEnum variants ->
            "(Json.Encode.string << " ++ enumFnBaseName variants ++ "ToString)"

        FNullable inner ->
            "(Maybe.map " ++ toValueEncoder inner ++ " >> Maybe.withDefault Json.Encode.null)"

        FList inner ->
            "(Json.Encode.list " ++ toValueEncoder inner ++ ")"

        FObject _ ->
            "identity"


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


enumFnBaseName : List String -> String
enumFnBaseName variants =
    lowercaseFirst (toElmType (FEnum variants))


enumFromStringFunction : List String -> String
enumFromStringFunction variants =
    let
        fnName =
            enumFnBaseName variants ++ "FromString"

        typeName =
            toElmType (FEnum variants)

        branches =
            List.map
                (\v ->
                    "        \"" ++ v ++ "\" ->\n            Ok " ++ capitalizeFirst v
                )
                variants

        catchAll =
            "        _ ->\n            Err (\"Unknown value: \" ++ str ++ \". Expected one of: " ++ String.join ", " variants ++ "\")"
    in
    fnName ++ " : String -> Result String " ++ typeName ++ "\n"
        ++ fnName ++ " str =\n"
        ++ "    case str of\n"
        ++ String.join "\n\n" (branches ++ [ catchAll ])


enumToStringFunction : List String -> String
enumToStringFunction variants =
    let
        fnName =
            enumFnBaseName variants ++ "ToString"

        typeName =
            toElmType (FEnum variants)

        branches =
            List.map
                (\v ->
                    "        " ++ capitalizeFirst v ++ " ->\n            \"" ++ v ++ "\""
                )
                variants
    in
    fnName ++ " : " ++ typeName ++ " -> String\n"
        ++ fnName ++ " value =\n"
        ++ "    case value of\n"
        ++ String.join "\n\n" branches


enumDecoderFunction : List String -> String
enumDecoderFunction variants =
    let
        fnName =
            enumFnBaseName variants ++ "Decoder"

        typeName =
            toElmType (FEnum variants)

        fromStringFn =
            enumFnBaseName variants ++ "FromString"
    in
    fnName ++ " : Decode.Decoder " ++ typeName ++ "\n"
        ++ fnName ++ " =\n"
        ++ "    Decode.string\n"
        ++ "        |> Decode.andThen\n"
        ++ "            (\\s ->\n"
        ++ "                case " ++ fromStringFn ++ " s of\n"
        ++ "                    Ok v ->\n"
        ++ "                        Decode.succeed v\n\n"
        ++ "                    Err e ->\n"
        ++ "                        Decode.fail e\n"
        ++ "            )"


lowercaseFirst : String -> String
lowercaseFirst s =
    case String.uncons s of
        Just ( first, rest ) ->
            String.fromChar (Char.toLower first) ++ rest

        Nothing ->
            s


capitalizeFirst : String -> String
capitalizeFirst s =
    case String.uncons s of
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest

        Nothing ->
            s
