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


cardWithObjectSchema : ComponentSchema
cardWithObjectSchema =
    { fields =
        Dict.fromList
            [ ( "title", { fieldType = FString, required = True } )
            , ( "meta"
              , { fieldType =
                    FObject
                        (Dict.fromList
                            [ ( "author", { fieldType = FString, required = True } )
                            , ( "count", { fieldType = FInt, required = False } )
                            ]
                        )
                , required = True
                }
              )
            ]
    , description = "A card with object field"
    , slots = []
    }


badgeSchema : ComponentSchema
badgeSchema =
    { fields =
        Dict.fromList
            [ ( "label", { fieldType = FString, required = True } )
            , ( "color", { fieldType = FEnum [ "green", "red", "blue" ], required = False } )
            ]
    , description = "A badge"
    , slots = []
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
                        ElmCodeGen.componentModule "Catalog" "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "module Catalog.Components.Card" c |> Expect.equal True
                    , \c -> String.contains "view ctx =" c |> Expect.equal True
                    , \c -> String.contains "()" c |> Expect.equal True
                    ]
                    code
        , test "generates registry module" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.registryModule "Catalog" [ "Card", "Button" ] False
                in
                Expect.all
                    [ \c -> String.contains "module Catalog.Registry" c |> Expect.equal True
                    , \c -> String.contains "import Catalog.Components.Card" c |> Expect.equal True
                    , \c -> String.contains "import Catalog.Components.Button" c |> Expect.equal True
                    , \c -> String.contains "( \"Card\", Catalog.Components.Card.component )" c |> Expect.equal True
                    ]
                    code
        , test "generates bindings type alias" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.bindingsTypeAlias "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "type alias CardBindings msg" c |> Expect.equal True
                    , \c -> String.contains "subtitle : Maybe (String -> EventHandle msg)" c |> Expect.equal True
                    , \c -> String.contains "title : Maybe (String -> EventHandle msg)" c |> Expect.equal True
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
                    , \c -> String.contains "|> Bind.bindableTyped \"subtitle\" Json.Encode.string" c |> Expect.equal True
                    , \c -> String.contains "|> Bind.bindableTyped \"title\" Json.Encode.string" c |> Expect.equal True
                    ]
                    code
        , test "component module includes bindings" <|
            \_ ->
                let
                    code =
                        ElmCodeGen.componentModule "Catalog" "Card" cardSchema
                in
                Expect.all
                    [ \c -> String.contains "CardBindings" c |> Expect.equal True
                    , \c -> String.contains "import JsonRender.Bind as Bind" c |> Expect.equal True
                    , \c -> String.contains "register propsDecoder bindingsDecoder view" c |> Expect.equal True
                    , \c -> String.contains "import JsonRender.Events exposing (EventHandle)" c |> Expect.equal True
                    , \c -> String.contains "ComponentContext CardProps (CardBindings msg) msg" c |> Expect.equal True
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
                    |> Expect.all
                        [ \c -> String.contains "| Press" c |> Expect.equal True
                        , \c -> String.contains "PressParams" c |> Expect.equal False
                        ]
        , test "generates actions module" <|
            \_ ->
                ElmCodeGen.actionsModule "Catalog" testActions
                    |> Expect.all
                        [ \c -> String.contains "module Catalog.Actions" c |> Expect.equal True
                        , \c -> String.contains "type Action" c |> Expect.equal True
                        , \c -> String.contains "type alias ExportParams" c |> Expect.equal True
                        , \c -> String.contains "decodeAction" c |> Expect.equal True
                        , \c -> String.contains "handleAction" c |> Expect.equal True
                        , \c -> String.contains "actionConfig" c |> Expect.equal True
                        ]
        , test "actions module exposes actionConfig, decodeAction, handleAction" <|
            \_ ->
                ElmCodeGen.actionsModule "Catalog" testActions
                    |> String.contains "exposing (Action(..), actionConfig, decodeAction, handleAction)"
                    |> Expect.equal True
        , test "actions module has correct imports" <|
            \_ ->
                ElmCodeGen.actionsModule "Catalog" testActions
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
        , test "component module with enum includes type declaration" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Badge" badgeSchema
                    |> Expect.all
                        [ \c -> String.contains "type GreenOrRedOrBlue\n    = Green\n    | Red\n    | Blue" c |> Expect.equal True
                        , \c -> String.contains "greenOrRedOrBlueFromString" c |> Expect.equal True
                        , \c -> String.contains "greenOrRedOrBlueToString" c |> Expect.equal True
                        ]
        , test "component module with enum has typed bindings" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Badge" badgeSchema
                    |> Expect.all
                        [ \c -> String.contains "color : Maybe (GreenOrRedOrBlue -> EventHandle msg)" c |> Expect.equal True
                        , \c -> String.contains "label : Maybe (String -> EventHandle msg)" c |> Expect.equal True
                        , \c -> String.contains "Bind.bindableTyped \"color\" (Json.Encode.string << greenOrRedOrBlueToString)" c |> Expect.equal True
                        , \c -> String.contains "Bind.bindableTyped \"label\" Json.Encode.string" c |> Expect.equal True
                        ]
        , test "component module with enum exposes type with constructors" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Badge" badgeSchema
                    |> String.contains "GreenOrRedOrBlue(..)"
                    |> Expect.equal True
        , test "component module enum props use composed extractor" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Badge" badgeSchema
                    |> String.contains "Result.andThen greenOrRedOrBlueFromString"
                    |> Expect.equal True
        , test "component module without enums has no enum declarations" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Card" cardSchema
                    |> String.contains "FromString"
                    |> Expect.equal False
        , test "component module with object field generates record type" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Card" cardWithObjectSchema
                    |> Expect.all
                        [ \c -> String.contains "type alias MetaObject" c |> Expect.equal True
                        , \c -> String.contains "author : String" c |> Expect.equal True
                        , \c -> String.contains "count : Maybe Int" c |> Expect.equal True
                        , \c -> String.contains "meta : MetaObject" c |> Expect.equal True
                        ]
        , test "component module with object field generates decoder and encoder" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Card" cardWithObjectSchema
                    |> Expect.all
                        [ \c -> String.contains "metaObjectDecoder" c |> Expect.equal True
                        , \c -> String.contains "metaObjectEncoder" c |> Expect.equal True
                        , \c -> String.contains "ResolvedValue.object" c |> Expect.equal True
                        , \c -> String.contains "Json.Encode.object" c |> Expect.equal True
                        ]
        , test "component module with object field has typed binding" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Card" cardWithObjectSchema
                    |> Expect.all
                        [ \c -> String.contains "meta : Maybe (MetaObject -> EventHandle msg)" c |> Expect.equal True
                        , \c -> String.contains "Bind.bindableTyped \"meta\" metaObjectEncoder" c |> Expect.equal True
                        ]
        , test "component module with object field exposes type" <|
            \_ ->
                ElmCodeGen.componentModule "Catalog" "Card" cardWithObjectSchema
                    |> String.contains "MetaObject"
                    |> Expect.equal True
        , test "actions module with enum params includes enum helpers with decoder" <|
            \_ ->
                let
                    enumActions =
                        Dict.fromList
                            [ ( "setTheme"
                              , { params =
                                    Dict.fromList
                                        [ ( "theme", { fieldType = FEnum [ "light", "dark" ], required = True } ) ]
                                , description = "Set theme"
                                }
                              )
                            ]
                in
                ElmCodeGen.actionsModule "Catalog" enumActions
                    |> Expect.all
                        [ \c -> String.contains "type LightOrDark\n    = Light\n    | Dark" c |> Expect.equal True
                        , \c -> String.contains "lightOrDarkFromString" c |> Expect.equal True
                        , \c -> String.contains "lightOrDarkToString" c |> Expect.equal True
                        , \c -> String.contains "lightOrDarkDecoder" c |> Expect.equal True
                        , \c -> String.contains "LightOrDark(..)" c |> Expect.equal True
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
                ElmCodeGen.actionsModule "Catalog" onlyEmptyActions
                    |> Expect.all
                        [ \c -> String.contains "type alias" c |> Expect.equal False
                        , \c -> String.contains "Ok Press" c |> Expect.equal True
                        , \c -> String.contains "Ok Click" c |> Expect.equal True
                        ]
        , test "generates managed comment listing all declarations" <|
            \_ ->
                ElmCodeGen.generatedComment "Card" cardSchema
                    |> Expect.equal
                        """{- This module was generated by the CatalogSync elm-review rule.
   These values were created by the rule, and will be overwritten by it if changed:
   - type alias CardProps
   - type alias CardBindings
   - propsDecoder
   - bindingsDecoder
   - component
-}"""
        , test "generated comment includes enum helpers" <|
            \_ ->
                ElmCodeGen.generatedComment "Badge" badgeSchema
                    |> Expect.equal
                        """{- This module was generated by the CatalogSync elm-review rule.
   These values were created by the rule, and will be overwritten by it if changed:
   - type GreenOrRedOrBlue
   - greenOrRedOrBlueFromString
   - greenOrRedOrBlueToString
   - type alias BadgeProps
   - type alias BadgeBindings
   - propsDecoder
   - bindingsDecoder
   - component
-}"""
        , test "generated comment includes object helpers" <|
            \_ ->
                ElmCodeGen.generatedComment "Card" cardWithObjectSchema
                    |> Expect.equal
                        """{- This module was generated by the CatalogSync elm-review rule.
   These values were created by the rule, and will be overwritten by it if changed:
   - type alias MetaObject
   - metaObjectDecoder
   - metaObjectEncoder
   - type alias CardProps
   - type alias CardBindings
   - propsDecoder
   - bindingsDecoder
   - component
-}"""
        , test "expectedDeclarations for simple component has 5 entries" <|
            \_ ->
                ElmCodeGen.expectedDeclarations "Card" cardSchema
                    |> List.length
                    |> Expect.equal 5
        , test "expectedDeclarations names are correct for simple component" <|
            \_ ->
                ElmCodeGen.expectedDeclarations "Card" cardSchema
                    |> List.map .name
                    |> Expect.equal [ "CardProps", "CardBindings", "propsDecoder", "bindingsDecoder", "component" ]
        , test "expectedDeclarations kinds are correct for simple component" <|
            \_ ->
                ElmCodeGen.expectedDeclarations "Card" cardSchema
                    |> List.map .kind
                    |> Expect.equal
                        [ ElmCodeGen.TypeAliasDecl
                        , ElmCodeGen.TypeAliasDecl
                        , ElmCodeGen.FunctionDecl
                        , ElmCodeGen.FunctionDecl
                        , ElmCodeGen.FunctionDecl
                        ]
        , test "expectedDeclarations includes enum helpers" <|
            \_ ->
                ElmCodeGen.expectedDeclarations "Badge" badgeSchema
                    |> List.map .name
                    |> Expect.equal
                        [ "GreenOrRedOrBlue"
                        , "greenOrRedOrBlueFromString"
                        , "greenOrRedOrBlueToString"
                        , "BadgeProps"
                        , "BadgeBindings"
                        , "propsDecoder"
                        , "bindingsDecoder"
                        , "component"
                        ]
        , test "expectedDeclarations includes object helpers" <|
            \_ ->
                ElmCodeGen.expectedDeclarations "Card" cardWithObjectSchema
                    |> List.map .name
                    |> Expect.equal
                        [ "MetaObject"
                        , "metaObjectDecoder"
                        , "metaObjectEncoder"
                        , "CardProps"
                        , "CardBindings"
                        , "propsDecoder"
                        , "bindingsDecoder"
                        , "component"
                        ]
        , test "expectedDeclarations enum kind is CustomTypeDecl" <|
            \_ ->
                ElmCodeGen.expectedDeclarations "Badge" badgeSchema
                    |> List.filter (\d -> d.name == "GreenOrRedOrBlue")
                    |> List.map .kind
                    |> Expect.equal [ ElmCodeGen.CustomTypeDecl ]
        , test "expectedDeclarations code for propsDecoder matches propsDecoder output" <|
            \_ ->
                let
                    decls =
                        ElmCodeGen.expectedDeclarations "Card" cardSchema

                    propsDecoderDecl =
                        List.filter (\d -> d.name == "propsDecoder") decls
                            |> List.head
                in
                case propsDecoderDecl of
                    Just d ->
                        d.code |> Expect.equal (ElmCodeGen.propsDecoder "Card" cardSchema)

                    Nothing ->
                        Expect.fail "propsDecoder not found in expectedDeclarations"
        ]
