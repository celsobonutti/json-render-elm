module TypeMappingTest exposing (..)

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
        ]
