module ElmCodeGenTest exposing (..)

import Dict
import Expect
import JsonRender.Internal.ElmCodeGen as ElmCodeGen
import JsonRender.Internal.SchemaParser exposing (ComponentSchema, FieldSchema, FieldType(..))
import Test exposing (..)


cardSchema : ComponentSchema
cardSchema =
    { fields =
        Dict.fromList
            [ ( "title", { fieldType = FString, required = True } )
            , ( "subtitle", { fieldType = FString, required = False } )
            ]
    , description = "A card container"
    , hasChildren = True
    }


suite : Test
suite =
    describe "ElmCodeGen"
        [ test "generates props type alias" <|
            \_ ->
                ElmCodeGen.propsTypeAlias "Card" cardSchema
                    |> String.contains "type alias CardProps"
                    |> Expect.equal True
        , test "props type has correct fields" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.propsTypeAlias "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "title : String" c |> Expect.equal True
                    , \c -> String.contains "subtitle : Maybe String" c |> Expect.equal True
                    ]
                    code
        , test "generates pipeline decoder" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.propsDecoder "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "ResolvedValue.succeed CardProps" c |> Expect.equal True
                    , \c -> String.contains "|> ResolvedValue.required \"title\"" c |> Expect.equal True
                    , \c -> String.contains "|> ResolvedValue.optional \"subtitle\"" c |> Expect.equal True
                    ]
                    code
        , test "generates full component module" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.componentModule "Components" "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "module Components.Card" c |> Expect.equal True
                    , \c -> String.contains "view ctx =" c |> Expect.equal True
                    , \c -> String.contains "()" c |> Expect.equal True
                    ]
                    code
        , test "generates registry module" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.registryModule "Components" [ "Card", "Button" ]
                in
                Expect.all
                    [ \c -> String.contains "module Components.Registry" c |> Expect.equal True
                    , \c -> String.contains "import Components.Card" c |> Expect.equal True
                    , \c -> String.contains "import Components.Button" c |> Expect.equal True
                    , \c -> String.contains "( \"Card\", Components.Card.component )" c |> Expect.equal True
                    ]
                    code
        ]
