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
                        ElmCodeGen.registryModule "Components" [ "Card", "Button" ] False
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
                        , \c -> String.contains "decodeAction" c |> Expect.equal True
                        , \c -> String.contains "handleAction" c |> Expect.equal True
                        , \c -> String.contains "actionConfig" c |> Expect.equal True
                        ]
        , test "actions module exposes actionConfig, decodeAction, handleAction" <|
            \_ ->
                ElmCodeGen.actionsModule "Components" testActions
                    |> String.contains "exposing (Action(..), actionConfig, decodeAction, handleAction)"
                    |> Expect.equal True
        , test "actions module has correct imports" <|
            \_ ->
                ElmCodeGen.actionsModule "Components" testActions
                    |> Expect.all
                        [ \c -> String.contains "import Dict exposing (Dict)" c |> Expect.equal True
                        , \c -> String.contains "import Json.Decode as Decode" c |> Expect.equal True
                        , \c -> String.contains "import Json.Encode exposing (Value)" c |> Expect.equal True
                        , \c -> String.contains "import JsonRender.Actions as Actions" c |> Expect.equal True
                        ]
        , test "generates handleAction with () placeholder" <|
            \_ ->
                ElmCodeGen.handleActionFunction
                    |> Expect.all
                        [ \c -> String.contains "handleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )" c |> Expect.equal True
                        , \c -> String.contains "handleAction action model =\n    ()" c |> Expect.equal True
                        ]
        , test "generates actionConfig with two fields" <|
            \_ ->
                ElmCodeGen.actionConfigFunction
                    |> Expect.all
                        [ \c -> String.contains "actionConfig : Actions.ActionConfig Action" c |> Expect.equal True
                        , \c -> String.contains "handleAction = handleAction" c |> Expect.equal True
                        , \c -> String.contains "decodeAction = decodeAction" c |> Expect.equal True
                        , \c -> String.contains "functions" c |> Expect.equal False
                        ]
        , test "generates decodeAction function signature" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "decodeAction : String -> Dict String Value -> Result String Action"
                    |> Expect.equal True
        , test "decodeAction has case on name" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "case name of"
                    |> Expect.equal True
        , test "decodeAction handles empty params action with Ok" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "\"press\" ->\n            Ok Press"
                    |> Expect.equal True
        , test "decodeAction handles parameterized action with Dict.get" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "Dict.get \"format\" params"
                    |> Expect.equal True
        , test "decodeAction decodes param value with Decode.decodeValue" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "Decode.decodeValue Decode.string format_raw"
                    |> Expect.equal True
        , test "decodeAction constructs record on success" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "Ok (Export { format = format })"
                    |> Expect.equal True
        , test "decodeAction has error for wrong type" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "Err \"format must be a String\""
                    |> Expect.equal True
        , test "decodeAction has error for missing param" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "Err \"missing required param format\""
                    |> Expect.equal True
        , test "decodeAction has catch-all for unknown actions" <|
            \_ ->
                ElmCodeGen.decodeActionFunction testActions
                    |> String.contains "Err (\"Unknown action: \" ++ name)"
                    |> Expect.equal True
        , test "decodeAction with multiple params" <|
            \_ ->
                let
                    multiParamAction =
                        Dict.fromList
                            [ ( "save"
                              , { params =
                                    Dict.fromList
                                        [ ( "filename", { fieldType = FString, required = True } )
                                        , ( "overwrite", { fieldType = FBool, required = True } )
                                        ]
                                , description = "Save file"
                                }
                              )
                            ]
                in
                ElmCodeGen.decodeActionFunction multiParamAction
                    |> Expect.all
                        [ \c -> String.contains "Dict.get \"filename\" params" c |> Expect.equal True
                        , \c -> String.contains "Dict.get \"overwrite\" params" c |> Expect.equal True
                        , \c -> String.contains "Decode.decodeValue Decode.string filename_raw" c |> Expect.equal True
                        , \c -> String.contains "Decode.decodeValue Decode.bool overwrite_raw" c |> Expect.equal True
                        , \c -> String.contains "Ok (Save { filename = filename, overwrite = overwrite })" c |> Expect.equal True
                        ]
        , test "decodeAction with optional params uses let binding" <|
            \_ ->
                let
                    optionalParamAction =
                        Dict.fromList
                            [ ( "notify"
                              , { params =
                                    Dict.fromList
                                        [ ( "message", { fieldType = FString, required = True } )
                                        , ( "channel", { fieldType = FString, required = False } )
                                        ]
                                , description = "Send notification"
                                }
                              )
                            ]
                in
                ElmCodeGen.decodeActionFunction optionalParamAction
                    |> Expect.all
                        [ \c -> String.contains "Dict.get \"message\" params" c |> Expect.equal True
                        , \c -> String.contains "channel =\n" c |> Expect.equal True
                        , \c -> String.contains "Maybe.andThen" c |> Expect.equal True
                        , \c -> String.contains "Result.toMaybe" c |> Expect.equal True
                        ]
        , test "decodeAction with integer param uses Decode.int" <|
            \_ ->
                let
                    intParamAction =
                        Dict.fromList
                            [ ( "resize"
                              , { params =
                                    Dict.fromList
                                        [ ( "width", { fieldType = FInt, required = True } ) ]
                                , description = "Resize"
                                }
                              )
                            ]
                in
                ElmCodeGen.decodeActionFunction intParamAction
                    |> Expect.all
                        [ \c -> String.contains "Decode.decodeValue Decode.int width_raw" c |> Expect.equal True
                        , \c -> String.contains "width must be a Int" c |> Expect.equal True
                        ]
        , test "decodeAction with no actions produces NoAction type and catch-all only" <|
            \_ ->
                let
                    emptyActions =
                        Dict.empty
                in
                ElmCodeGen.decodeActionFunction emptyActions
                    |> Expect.all
                        [ \c -> String.contains "case name of" c |> Expect.equal True
                        , \c -> String.contains "Err (\"Unknown action: \" ++ name)" c |> Expect.equal True
                        ]
        , test "actions module with only empty-param actions has no params type aliases" <|
            \_ ->
                let
                    onlyEmptyActions =
                        Dict.fromList
                            [ ( "press", pressAction )
                            , ( "click", { params = Dict.empty, description = "Click" } )
                            ]
                in
                ElmCodeGen.actionsModule "Components" onlyEmptyActions
                    |> Expect.all
                        [ \c -> String.contains "type alias" c |> Expect.equal False
                        , \c -> String.contains "Ok Press" c |> Expect.equal True
                        , \c -> String.contains "Ok Click" c |> Expect.equal True
                        ]
        ]
