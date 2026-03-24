module JsonRender.Internal.ElmCodeGen exposing
    ( bindingsDecoder
    , bindingsTypeAlias
    , componentModule
    , propsDecoder
    , propsTypeAlias
    , registryModule
    )

import Dict exposing (Dict)
import JsonRender.Internal.SchemaParser exposing (ComponentSchema, FieldSchema, FieldType(..))
import JsonRender.Internal.TypeMapping as TypeMapping


propsTypeAlias : String -> ComponentSchema -> String
propsTypeAlias componentName schema =
    let
        fields =
            Dict.toList schema.fields
                |> List.sortBy Tuple.first
                |> List.map
                    (\( name, field ) ->
                        let
                            elmType =
                                if field.required then
                                    TypeMapping.toElmType field.fieldType

                                else
                                    "Maybe " ++ TypeMapping.toElmType field.fieldType
                        in
                        name ++ " : " ++ elmType
                    )

        body =
            case fields of
                [] ->
                    "    {}"

                first :: rest ->
                    "    { "
                        ++ first
                        ++ String.concat (List.map (\f -> "\n    , " ++ f) rest)
                        ++ "\n    }"
    in
    "type alias " ++ componentName ++ "Props =\n" ++ body


propsDecoder : String -> ComponentSchema -> String
propsDecoder componentName schema =
    let
        fields =
            Dict.toList schema.fields
                |> List.sortBy Tuple.first

        pipelineSteps =
            List.map
                (\( name, field ) ->
                    let
                        extractor =
                            TypeMapping.toResolvedValueExtractor field.fieldType
                    in
                    if field.required then
                        "        |> ResolvedValue.required \"" ++ name ++ "\" " ++ extractor

                    else
                        "        |> ResolvedValue.optional \"" ++ name ++ "\" " ++ extractor ++ " Nothing"
                )
                fields
    in
    "propsDecoder : Dict String ResolvedValue -> Result String "
        ++ componentName
        ++ "Props\npropsDecoder =\n"
        ++ "    ResolvedValue.succeed "
        ++ componentName
        ++ "Props\n"
        ++ String.join "\n" pipelineSteps


bindingsTypeAlias : String -> ComponentSchema -> String
bindingsTypeAlias componentName schema =
    let
        fields =
            Dict.keys schema.fields
                |> List.sort
                |> List.map (\name -> name ++ " : Maybe (Value -> Msg)")

        body =
            case fields of
                [] ->
                    "    {}"

                first :: rest ->
                    "    { "
                        ++ first
                        ++ String.concat (List.map (\f -> "\n    , " ++ f) rest)
                        ++ "\n    }"
    in
    "type alias " ++ componentName ++ "Bindings =\n" ++ body


bindingsDecoder : String -> ComponentSchema -> String
bindingsDecoder componentName schema =
    let
        fields =
            Dict.keys schema.fields
                |> List.sort

        pipelineSteps =
            List.map
                (\name -> "        |> Bind.bindable \"" ++ name ++ "\"")
                fields
    in
    "bindingsDecoder : Dict String (Value -> Msg) -> "
        ++ componentName
        ++ "Bindings\nbindingsDecoder =\n"
        ++ "    Bind.succeed "
        ++ componentName
        ++ "Bindings\n"
        ++ String.join "\n" pipelineSteps


componentModule : String -> String -> ComponentSchema -> String
componentModule namespace componentName schema =
    let
        moduleName =
            namespace ++ "." ++ componentName

        typeAlias =
            propsTypeAlias componentName schema

        decoderCode =
            propsDecoder componentName schema

        bindingsType =
            bindingsTypeAlias componentName schema

        bindingsDecoderCode =
            bindingsDecoder componentName schema
    in
    "module "
        ++ moduleName
        ++ " exposing ("
        ++ componentName
        ++ "Props, "
        ++ componentName
        ++ "Bindings, propsDecoder, bindingsDecoder, component)\n\n"
        ++ "import Dict exposing (Dict)\n"
        ++ "import Html exposing (Html)\n"
        ++ "import Json.Encode exposing (Value)\n"
        ++ "import JsonRender.Actions exposing (Msg)\n"
        ++ "import JsonRender.Bind as Bind\n"
        ++ "import JsonRender.Render exposing (ComponentContext, Component, register)\n"
        ++ "import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)\n\n\n"
        ++ typeAlias
        ++ "\n\n\n"
        ++ bindingsType
        ++ "\n\n\n"
        ++ decoderCode
        ++ "\n\n\n"
        ++ bindingsDecoderCode
        ++ "\n\n\n"
        ++ "component : Component\ncomponent =\n    register propsDecoder bindingsDecoder view\n\n\n"
        ++ "view : ComponentContext "
        ++ componentName
        ++ "Props "
        ++ componentName
        ++ "Bindings -> Html Msg\nview ctx =\n    ()\n"


registryModule : String -> List String -> String
registryModule namespace componentNames =
    let
        imports =
            List.map
                (\name -> "import " ++ namespace ++ "." ++ name)
                componentNames

        entries =
            List.map
                (\name ->
                    "        ( \""
                        ++ name
                        ++ "\", "
                        ++ namespace
                        ++ "."
                        ++ name
                        ++ ".component )"
                )
                componentNames

        entriesStr =
            case entries of
                [] ->
                    "        []"

                first :: rest ->
                    "        [ "
                        ++ String.trimLeft first
                        ++ String.concat (List.map (\e -> "\n        , " ++ String.trimLeft e) rest)
                        ++ "\n        ]"
    in
    "module "
        ++ namespace
        ++ ".Registry exposing (registry)\n\n"
        ++ "import Dict\n"
        ++ "import JsonRender.Render exposing (Registry)\n"
        ++ String.join "\n" imports
        ++ "\n\n\n"
        ++ "registry : Registry\nregistry =\n    Dict.fromList\n"
        ++ entriesStr
        ++ "\n"
