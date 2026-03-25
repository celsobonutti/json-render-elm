module ElmCodeGenTest exposing (..)

import Dict
import Expect
import JsonRender.Internal.ElmCodeGen as ElmCodeGen
import JsonRender.Internal.SchemaParser exposing (ActionSchema, ComponentSchema, FieldType(..))
import Test exposing (..)


cardSchema : ComponentSchema
cardSchema =
    { fields =
        Dict.fromList
            [ ( "title", { fieldType = FString, required = True } )
            , ( "subtitle", { fieldType = FString, required = False } )
            ]
    , description = "A card container"
    , slots = [ "default" ]
    }


exportAction : ActionSchema
exportAction =
    { params =
        Dict.fromList
            [ ( "format", { fieldType = FString, required = True } ) ]
    , description = "Export data"
    }


pressAction : ActionSchema
pressAction =
    { params = Dict.empty
    , description = "Generic button press"
    }


testActions : Dict.Dict String ActionSchema
testActions =
    Dict.fromList
        [ ( "export", exportAction )
        , ( "press", pressAction )
        ]


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
        , test "generates bindings type alias" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.bindingsTypeAlias "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "type alias CardBindings" c |> Expect.equal True
                    , \c -> String.contains "subtitle : Maybe (Value -> Msg)" c |> Expect.equal True
                    , \c -> String.contains "title : Maybe (Value -> Msg)" c |> Expect.equal True
                    ]
                    code
        , test "generates bindings decoder" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.bindingsDecoder "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "Bind.succeed CardBindings" c |> Expect.equal True
                    , \c -> String.contains "|> Bind.bindable \"subtitle\"" c |> Expect.equal True
                    , \c -> String.contains "|> Bind.bindable \"title\"" c |> Expect.equal True
                    ]
                    code
        , test "component module includes bindings" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.componentModule "Components" "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "CardBindings" c |> Expect.equal True
                    , \c -> String.contains "import JsonRender.Bind as Bind" c |> Expect.equal True
                    , \c -> String.contains "register propsDecoder bindingsDecoder view" c |> Expect.equal True
                    , \c -> String.contains "ComponentContext CardProps CardBindings" c |> Expect.equal True
                    ]
                    code
        , test "generates action params type alias" <|
            \_ ->
                ElmCodeGen.actionParamsType "Export" exportAction
                    |> Expect.all
                        [ \c -> String.contains "type alias ExportParams" c |> Expect.equal True
                        , \c -> String.contains "format : String" c |> Expect.equal True
                        ]
        , test "generates Action union type" <|
            \_ ->
                ElmCodeGen.actionType testActions
                    |> Expect.all
                        [ \c -> String.contains "type Action" c |> Expect.equal True
                        , \c -> String.contains "Export ExportParams" c |> Expect.equal True
                        , \c -> String.contains "Press" c |> Expect.equal True
                        ]
        , test "action with empty params has no payload" <|
            \_ ->
                ElmCodeGen.actionType testActions
                    |> String.contains "| Press\n"
                    |> Expect.equal True
        , test "generates actions module" <|
            \_ ->
                ElmCodeGen.actionsModule "Components" testActions
                    |> Expect.all
                        [ \c -> String.contains "module Components.Actions" c |> Expect.equal True
                        , \c -> String.contains "type Action" c |> Expect.equal True
                        , \c -> String.contains "type alias ExportParams" c |> Expect.equal True
                        ]
        ]
