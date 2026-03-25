module JsonRender.Internal.ElmCodeGen exposing
    ( actionParamsType
    , actionType
    , actionsModule
    , bindingsDecoder
    , bindingsTypeAlias
    , componentModule
    , decodeActionFunction
    , propsDecoder
    , propsTypeAlias
    , registryModule
    )

import Dict exposing (Dict)
import JsonRender.Internal.SchemaParser as SchemaParser exposing (ActionSchema, ComponentSchema)
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


actionParamsType : String -> ActionSchema -> String
actionParamsType actionName schema =
    let
        fields =
            Dict.toList schema.params
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
    "type alias " ++ actionName ++ "Params =\n" ++ body


actionType : Dict String ActionSchema -> String
actionType actions =
    let
        variants =
            Dict.toList actions
                |> List.sortBy Tuple.first
                |> List.map
                    (\( name, schema ) ->
                        let
                            capitalName =
                                TypeMapping.capitalizeFirst name
                        in
                        if Dict.isEmpty schema.params then
                            capitalName

                        else
                            capitalName ++ " " ++ capitalName ++ "Params"
                    )
    in
    case variants of
        [] ->
            "type Action\n    = NoAction"

        first :: rest ->
            "type Action\n    = "
                ++ first
                ++ String.concat (List.map (\v -> "\n    | " ++ v) rest)
                ++ "\n"


decodeActionFunction : Dict String ActionSchema -> String
decodeActionFunction actions =
    let
        sortedActions =
            Dict.toList actions
                |> List.sortBy Tuple.first

        branches =
            List.map actionBranch sortedActions

        catchAll =
            "        _ ->\n            Err (\"Unknown action: \" ++ name)"

        allBranches =
            branches ++ [ catchAll ]
    in
    "decodeAction : String -> Dict String Value -> Result String Action\n"
        ++ "decodeAction name params =\n"
        ++ "    case name of\n"
        ++ String.join "\n\n" allBranches


actionBranch : ( String, ActionSchema ) -> String
actionBranch ( name, schema ) =
    let
        capitalName =
            TypeMapping.capitalizeFirst name

        sortedParams =
            Dict.toList schema.params
                |> List.sortBy Tuple.first
                |> List.filter (\( _, field ) -> field.required)

        optionalParams =
            Dict.toList schema.params
                |> List.sortBy Tuple.first
                |> List.filter (\( _, field ) -> not field.required)
    in
    if Dict.isEmpty schema.params then
        "        \"" ++ name ++ "\" ->\n            Ok " ++ capitalName

    else
        let
            allParams =
                sortedParams ++ optionalParams

            recordFields =
                List.map
                    (\( pName, _ ) -> pName ++ " = " ++ pName)
                    allParams
                    |> String.join ", "

            successExpr =
                "Ok (" ++ capitalName ++ " { " ++ recordFields ++ " })"
        in
        "        \""
            ++ name
            ++ "\" ->\n"
            ++ nestedParamDecoding allParams successExpr 12


nestedParamDecoding : List ( String, { a | fieldType : SchemaParser.FieldType, required : Bool } ) -> String -> Int -> String
nestedParamDecoding params successExpr baseIndent =
    case params of
        [] ->
            indent baseIndent ++ successExpr

        ( pName, field ) :: rest ->
            let
                decoderExpr =
                    TypeMapping.toJsonDecoder field.fieldType

                currentIndent =
                    indent baseIndent

                innerIndent =
                    indent (baseIndent + 4)

                innerInnerIndent =
                    indent (baseIndent + 8)
            in
            if field.required then
                currentIndent
                    ++ "case Dict.get \""
                    ++ pName
                    ++ "\" params of\n"
                    ++ innerIndent
                    ++ "Just "
                    ++ pName
                    ++ "_raw ->\n"
                    ++ innerInnerIndent
                    ++ "case Decode.decodeValue "
                    ++ decoderExpr
                    ++ " "
                    ++ pName
                    ++ "_raw of\n"
                    ++ indent (baseIndent + 12)
                    ++ "Ok "
                    ++ pName
                    ++ " ->\n"
                    ++ nestedParamDecoding rest successExpr (baseIndent + 16)
                    ++ "\n\n"
                    ++ indent (baseIndent + 12)
                    ++ "Err _ ->\n"
                    ++ indent (baseIndent + 16)
                    ++ "Err \""
                    ++ pName
                    ++ " must be a "
                    ++ TypeMapping.toElmType field.fieldType
                    ++ "\"\n\n"
                    ++ innerIndent
                    ++ "Nothing ->\n"
                    ++ innerInnerIndent
                    ++ "Err \"missing required param "
                    ++ pName
                    ++ "\""

            else
                currentIndent
                    ++ "let\n"
                    ++ innerIndent
                    ++ pName
                    ++ " =\n"
                    ++ innerInnerIndent
                    ++ "Dict.get \""
                    ++ pName
                    ++ "\" params\n"
                    ++ innerInnerIndent
                    ++ "|> Maybe.andThen (\\raw -> Decode.decodeValue "
                    ++ decoderExpr
                    ++ " raw |> Result.toMaybe)\n"
                    ++ currentIndent
                    ++ "in\n"
                    ++ nestedParamDecoding rest successExpr baseIndent


indent : Int -> String
indent n =
    String.repeat n " "


actionsModule : String -> Dict String ActionSchema -> String
actionsModule namespace actions =
    let
        paramsTypes =
            Dict.toList actions
                |> List.sortBy Tuple.first
                |> List.filterMap
                    (\( name, schema ) ->
                        if Dict.isEmpty schema.params then
                            Nothing

                        else
                            Just (actionParamsType (TypeMapping.capitalizeFirst name) schema)
                    )

        paramsTypesStr =
            case paramsTypes of
                [] ->
                    ""

                _ ->
                    String.join "\n\n\n" paramsTypes ++ "\n\n\n"

        imports =
            "import Dict exposing (Dict)\n"
                ++ "import Json.Decode as Decode\n"
                ++ "import Json.Encode exposing (Value)\n"
    in
    "module "
        ++ namespace
        ++ ".Actions exposing (Action(..), decodeAction)\n\n"
        ++ imports
        ++ "\n\n"
        ++ paramsTypesStr
        ++ actionType actions
        ++ "\n\n"
        ++ decodeActionFunction actions
        ++ "\n"


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
