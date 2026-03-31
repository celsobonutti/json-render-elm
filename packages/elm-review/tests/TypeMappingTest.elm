module TypeMappingTest exposing (..)

import Dict
import Expect
import JsonRender.Internal.SchemaParser exposing (FieldType(..))
import JsonRender.Internal.TypeMapping as TypeMapping
import Test exposing (..)


suite : Test
suite =
    describe "TypeMapping"
        [ test "maps string to String" <|
            \_ ->
                TypeMapping.toElmType FString
                    |> Expect.equal "String"
        , test "maps integer to Int" <|
            \_ ->
                TypeMapping.toElmType FInt
                    |> Expect.equal "Int"
        , test "maps number to Float" <|
            \_ ->
                TypeMapping.toElmType FFloat
                    |> Expect.equal "Float"
        , test "maps boolean to Bool" <|
            \_ ->
                TypeMapping.toElmType FBool
                    |> Expect.equal "Bool"
        , test "maps nullable string to Maybe String" <|
            \_ ->
                TypeMapping.toElmType (FNullable FString)
                    |> Expect.equal "Maybe String"
        , test "maps array of string to List String" <|
            \_ ->
                TypeMapping.toElmType (FList FString)
                    |> Expect.equal "List String"
        , test "maps enum to placeholder name (caller provides real name)" <|
            \_ ->
                TypeMapping.toElmType (FEnum [ "left", "center", "right" ])
                    |> Expect.equal "LeftOrCenterOrRight"
        , test "generates custom type declaration for enum" <|
            \_ ->
                TypeMapping.enumTypeDeclaration "Alignment" [ "left", "center", "right" ]
                    |> Expect.equal "type Alignment\n    = Left\n    | Center\n    | Right"
        , test "toResolvedValueExtractor maps types to extractors" <|
            \_ ->
                TypeMapping.toResolvedValueExtractor FString
                    |> Expect.equal "ResolvedValue.string"
        , test "toJsonDecoder maps string to Decode.string" <|
            \_ ->
                TypeMapping.toJsonDecoder FString
                    |> Expect.equal "Decode.string"
        , test "toJsonDecoder maps integer to Decode.int" <|
            \_ ->
                TypeMapping.toJsonDecoder FInt
                    |> Expect.equal "Decode.int"
        , test "toJsonDecoder maps number to Decode.float" <|
            \_ ->
                TypeMapping.toJsonDecoder FFloat
                    |> Expect.equal "Decode.float"
        , test "toJsonDecoder maps boolean to Decode.bool" <|
            \_ ->
                TypeMapping.toJsonDecoder FBool
                    |> Expect.equal "Decode.bool"
        , test "toJsonDecoder maps nullable to Decode.nullable" <|
            \_ ->
                TypeMapping.toJsonDecoder (FNullable FString)
                    |> Expect.equal "(Decode.nullable Decode.string)"
        , test "toJsonDecoder maps list to Decode.list" <|
            \_ ->
                TypeMapping.toJsonDecoder (FList FInt)
                    |> Expect.equal "(Decode.list Decode.int)"
        , test "toJsonDecoder maps enum to named decoder" <|
            \_ ->
                TypeMapping.toJsonDecoder (FEnum [ "a", "b" ])
                    |> Expect.equal "aOrBDecoder"
        , test "toResolvedValueExtractor maps enum to composed extractor" <|
            \_ ->
                TypeMapping.toResolvedValueExtractor (FEnum [ "left", "right" ])
                    |> Expect.equal "(\\rv -> ResolvedValue.string rv |> Result.andThen leftOrRightFromString)"
        , test "enumFnBaseName lowercases first char of type name" <|
            \_ ->
                TypeMapping.enumFnBaseName [ "primary", "secondary" ]
                    |> Expect.equal "primaryOrSecondary"
        , test "enumFromStringFunction generates case expression" <|
            \_ ->
                TypeMapping.enumFromStringFunction [ "a", "b" ]
                    |> Expect.all
                        [ \c -> String.contains "aOrBFromString : String -> Result String AOrB" c |> Expect.equal True
                        , \c -> String.contains "\"a\" ->\n            Ok A" c |> Expect.equal True
                        , \c -> String.contains "\"b\" ->\n            Ok B" c |> Expect.equal True
                        , \c -> String.contains "Expected one of: a, b" c |> Expect.equal True
                        ]
        , test "enumToStringFunction generates case expression" <|
            \_ ->
                TypeMapping.enumToStringFunction [ "a", "b" ]
                    |> Expect.all
                        [ \c -> String.contains "aOrBToString : AOrB -> String" c |> Expect.equal True
                        , \c -> String.contains "A ->\n            \"a\"" c |> Expect.equal True
                        , \c -> String.contains "B ->\n            \"b\"" c |> Expect.equal True
                        ]
        , test "enumDecoderFunction generates Decode.andThen pipeline" <|
            \_ ->
                TypeMapping.enumDecoderFunction [ "x", "y" ]
                    |> Expect.all
                        [ \c -> String.contains "xOrYDecoder : Decode.Decoder XOrY" c |> Expect.equal True
                        , \c -> String.contains "Decode.string" c |> Expect.equal True
                        , \c -> String.contains "xOrYFromString" c |> Expect.equal True
                        ]
        , test "toJsonDecoder maps object to Decode.value" <|
            \_ ->
                TypeMapping.toJsonDecoder (FObject Dict.empty)
                    |> Expect.equal "Decode.value"
        , test "toValueEncoder maps string to Json.Encode.string" <|
            \_ ->
                TypeMapping.toValueEncoder FString
                    |> Expect.equal "Json.Encode.string"
        , test "toValueEncoder maps int to Json.Encode.int" <|
            \_ ->
                TypeMapping.toValueEncoder FInt
                    |> Expect.equal "Json.Encode.int"
        , test "toValueEncoder maps bool to Json.Encode.bool" <|
            \_ ->
                TypeMapping.toValueEncoder FBool
                    |> Expect.equal "Json.Encode.bool"
        , test "toValueEncoder maps enum to toString composition" <|
            \_ ->
                TypeMapping.toValueEncoder (FEnum [ "a", "b" ])
                    |> Expect.equal "(Json.Encode.string << aOrBToString)"
        , test "toValueEncoder maps list to Json.Encode.list" <|
            \_ ->
                TypeMapping.toValueEncoder (FList FInt)
                    |> Expect.equal "(Json.Encode.list Json.Encode.int)"
        , test "toValueEncoder maps object to identity" <|
            \_ ->
                TypeMapping.toValueEncoder (FObject Dict.empty)
                    |> Expect.equal "identity"
        ]
