module JsonRender.Internal.ElmCodeGen exposing
    ( actionConfigFunction
    , actionParamsType
    , actionType
    , actionsModule
    , bindingsDecoder
    , bindingsTypeAlias
    , componentBody
    , componentModule
    , componentScaffold
    , decodeActionFunction
    , defaultViewFunction
    , functionsModule
    , handleActionFunction
    , propsDecoder
    , propsTypeAlias
    , registryModule
    )

import Dict exposing (Dict)
import JsonRender.Internal.SchemaParser as SchemaParser exposing (ActionSchema, ComponentSchema, FieldType(..))
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
                                    fieldToElmType name field.fieldType

                                else
                                    "Maybe " ++ fieldToElmType name field.fieldType
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
                            fieldToExtractor name field.fieldType
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
            Dict.toList schema.fields
                |> List.sortBy Tuple.first
                |> List.map
                    (\( name, field ) ->
                        name ++ " : Maybe (" ++ fieldToElmType name field.fieldType ++ " -> EventHandle msg)"
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
    "type alias " ++ componentName ++ "Bindings msg =\n" ++ body


bindingsDecoder : String -> ComponentSchema -> String
bindingsDecoder componentName schema =
    let
        fields =
            Dict.toList schema.fields
                |> List.sortBy Tuple.first

        pipelineSteps =
            List.map
                (\( name, field ) ->
                    "        |> Bind.bindableTyped \"" ++ name ++ "\" " ++ fieldToEncoder name field.fieldType
                )
                fields
    in
    "bindingsDecoder : Dict String (Value -> EventHandle msg) -> "
        ++ componentName
        ++ "Bindings msg\nbindingsDecoder =\n"
        ++ "    Bind.succeed "
        ++ componentName
        ++ "Bindings\n"
        ++ String.join "\n" pipelineSteps


componentBody : String -> ComponentSchema -> String
componentBody componentName schema =
    let
        enums =
            collectEnumsFromFields schema.fields

        objects =
            collectObjectFields schema.fields

        typeAlias =
            propsTypeAlias componentName schema

        decoderCode =
            propsDecoder componentName schema

        bindingsType =
            bindingsTypeAlias componentName schema

        bindingsDecoderCode =
            bindingsDecoder componentName schema

        enumSection =
            case enums of
                [] ->
                    ""

                _ ->
                    List.map enumHelpers enums
                        |> String.join "\n\n\n"
                        |> (\s -> s ++ "\n\n\n")

        objectSection =
            case objects of
                [] ->
                    ""

                _ ->
                    List.map objectHelpers objects
                        |> String.join "\n\n\n"
                        |> (\s -> s ++ "\n\n\n")
    in
    enumSection
        ++ objectSection
        ++ typeAlias
        ++ "\n\n\n"
        ++ bindingsType
        ++ "\n\n\n"
        ++ decoderCode
        ++ "\n\n\n"
        ++ bindingsDecoderCode
        ++ "\n\n\n"
        ++ "component : Component msg\ncomponent =\n    register propsDecoder bindingsDecoder view"


componentScaffold : String -> String -> ComponentSchema -> String
componentScaffold namespace componentName schema =
    let
        moduleName =
            namespace ++ ".Components." ++ componentName

        enums =
            collectEnumsFromFields schema.fields

        objects =
            collectObjectFields schema.fields

        enumExposing =
            List.map (\variants -> TypeMapping.toElmType (FEnum variants) ++ "(..)") enums

        objectExposing =
            List.map (\( name, _ ) -> objectTypeName name) objects

        exposingItems =
            List.sort
                (enumExposing
                    ++ objectExposing
                    ++ [ componentName ++ "Props"
                       , componentName ++ "Bindings"
                       , "propsDecoder"
                       , "bindingsDecoder"
                       , "component"
                       ]
                )
    in
    "module "
        ++ moduleName
        ++ " exposing ("
        ++ String.join ", " exposingItems
        ++ ")\n\n"
        ++ "import Dict exposing (Dict)\n"
        ++ "import Html exposing (Html)\n"
        ++ "import Json.Encode exposing (Value)\n"
        ++ "import JsonRender.Bind as Bind\n"
        ++ "import JsonRender.Events exposing (EventHandle)\n"
        ++ "import JsonRender.Render exposing (Component, ComponentContext, register)\n"
        ++ "import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)\n\n\n"
        ++ componentBody componentName schema


defaultViewFunction : String -> String
defaultViewFunction componentName =
    "view : ComponentContext "
        ++ componentName
        ++ "Props ("
        ++ componentName
        ++ "Bindings msg) msg -> Html msg\nview ctx =\n    ()\n"


componentModule : String -> String -> ComponentSchema -> String
componentModule namespace componentName schema =
    componentScaffold namespace componentName schema
        ++ "\n\n\n"
        ++ defaultViewFunction componentName


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


collectEnumsFromFields : Dict String SchemaParser.FieldSchema -> List (List String)
collectEnumsFromFields fields =
    Dict.values fields
        |> List.concatMap (collectEnumsFromFieldType << .fieldType)
        |> dedupLists


collectEnumsFromFieldType : SchemaParser.FieldType -> List (List String)
collectEnumsFromFieldType fieldType =
    case fieldType of
        FEnum variants ->
            [ variants ]

        FObject subFields ->
            Dict.values subFields
                |> List.concatMap (collectEnumsFromFieldType << .fieldType)

        _ ->
            []


dedupLists : List (List String) -> List (List String)
dedupLists lists =
    List.foldl
        (\item acc ->
            if List.member item acc then
                acc

            else
                acc ++ [ item ]
        )
        []
        lists


enumHelpers : List String -> String
enumHelpers variants =
    let
        typeName =
            TypeMapping.toElmType (FEnum variants)
    in
    TypeMapping.enumTypeDeclaration typeName variants
        ++ "\n\n\n"
        ++ TypeMapping.enumFromStringFunction variants
        ++ "\n\n\n"
        ++ TypeMapping.enumToStringFunction variants


enumHelpersWithDecoder : List String -> String
enumHelpersWithDecoder variants =
    enumHelpers variants
        ++ "\n\n\n"
        ++ TypeMapping.enumDecoderFunction variants


collectObjectFields : Dict String SchemaParser.FieldSchema -> List ( String, Dict String SchemaParser.FieldSchema )
collectObjectFields fields =
    Dict.toList fields
        |> List.filterMap
            (\( name, field ) ->
                case field.fieldType of
                    FObject subFields ->
                        Just ( name, subFields )

                    _ ->
                        Nothing
            )


objectTypeName : String -> String
objectTypeName fieldName =
    TypeMapping.capitalizeFirst fieldName ++ "Object"


objectFnBaseName : String -> String
objectFnBaseName fieldName =
    fieldName ++ "Object"


objectHelpers : ( String, Dict String SchemaParser.FieldSchema ) -> String
objectHelpers ( fieldName, subFields ) =
    let
        typeName =
            objectTypeName fieldName

        baseName =
            objectFnBaseName fieldName

        sortedFields =
            Dict.toList subFields |> List.sortBy Tuple.first

        -- type alias
        typeFields =
            List.map
                (\( name, field ) ->
                    let
                        elmType =
                            if field.required then
                                fieldToElmType name field.fieldType

                            else
                                "Maybe " ++ fieldToElmType name field.fieldType
                    in
                    name ++ " : " ++ elmType
                )
                sortedFields

        typeBody =
            case typeFields of
                [] ->
                    "    {}"

                first :: rest ->
                    "    { "
                        ++ first
                        ++ String.concat (List.map (\f -> "\n    , " ++ f) rest)
                        ++ "\n    }"

        typeAliasCode =
            "type alias " ++ typeName ++ " =\n" ++ typeBody

        -- decoder (ResolvedValue pipeline)
        decoderSteps =
            List.map
                (\( name, field ) ->
                    let
                        extractor =
                            fieldToExtractor name field.fieldType
                    in
                    if field.required then
                        "        |> ResolvedValue.required \"" ++ name ++ "\" " ++ extractor

                    else
                        "        |> ResolvedValue.optional \"" ++ name ++ "\" " ++ extractor ++ " Nothing"
                )
                sortedFields

        decoderCode =
            baseName
                ++ "Decoder : Dict String ResolvedValue -> Result String "
                ++ typeName
                ++ "\n"
                ++ baseName
                ++ "Decoder =\n"
                ++ "    ResolvedValue.succeed "
                ++ typeName
                ++ "\n"
                ++ String.join "\n" decoderSteps

        -- encoder
        encoderFields =
            List.map
                (\( name, field ) ->
                    let
                        encoder =
                            fieldToEncoder name field.fieldType
                    in
                    if field.required then
                        "        ( \"" ++ name ++ "\", " ++ encoder ++ " record." ++ name ++ " )"

                    else
                        "        ( \""
                            ++ name
                            ++ "\", record."
                            ++ name
                            ++ " |> Maybe.map "
                            ++ encoder
                            ++ " |> Maybe.withDefault Json.Encode.null )"
                )
                sortedFields

        encoderBody =
            case encoderFields of
                [] ->
                    "    Json.Encode.object []"

                _ ->
                    "    Json.Encode.object\n"
                        ++ "        [ "
                        ++ String.join "\n        , " (List.map String.trimLeft encoderFields)
                        ++ "\n        ]"

        encoderCode =
            baseName
                ++ "Encoder : "
                ++ typeName
                ++ " -> Value\n"
                ++ baseName
                ++ "Encoder record =\n"
                ++ encoderBody
    in
    typeAliasCode ++ "\n\n\n" ++ decoderCode ++ "\n\n\n" ++ encoderCode


fieldToElmType : String -> SchemaParser.FieldType -> String
fieldToElmType fieldName fieldType =
    case fieldType of
        FObject _ ->
            objectTypeName fieldName

        _ ->
            TypeMapping.toElmType fieldType


fieldToExtractor : String -> SchemaParser.FieldType -> String
fieldToExtractor fieldName fieldType =
    case fieldType of
        FObject _ ->
            "(\\rv -> ResolvedValue.object rv |> Result.andThen " ++ objectFnBaseName fieldName ++ "Decoder)"

        _ ->
            TypeMapping.toResolvedValueExtractor fieldType


fieldToEncoder : String -> SchemaParser.FieldType -> String
fieldToEncoder fieldName fieldType =
    case fieldType of
        FObject _ ->
            objectFnBaseName fieldName ++ "Encoder"

        _ ->
            TypeMapping.toValueEncoder fieldType


handleActionFunction : String
handleActionFunction =
    "handleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )\n"
        ++ "handleAction action model =\n"
        ++ "    ()\n"


actionConfigFunction : String
actionConfigFunction =
    "actionConfig : Actions.ActionConfig Action\n"
        ++ "actionConfig =\n"
        ++ "    { handleAction = handleAction\n"
        ++ "    , decodeAction = decodeAction\n"
        ++ "    }\n"


actionsModule : String -> Dict String ActionSchema -> String
actionsModule namespace actions =
    let
        enums =
            Dict.values actions
                |> List.concatMap (\schema -> collectEnumsFromFields schema.params)
                |> dedupLists

        enumExposing =
            List.map (\variants -> TypeMapping.toElmType (FEnum variants) ++ "(..)") enums

        exposingItems =
            enumExposing
                ++ [ "Action(..)"
                   , "actionConfig"
                   , "decodeAction"
                   , "handleAction"
                   ]

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

        enumSection =
            case enums of
                [] ->
                    ""

                _ ->
                    List.map enumHelpersWithDecoder enums
                        |> String.join "\n\n\n"
                        |> (\s -> s ++ "\n\n\n")

        imports =
            "import Dict exposing (Dict)\n"
                ++ "import Json.Decode as Decode\n"
                ++ "import Json.Encode exposing (Value)\n"
                ++ "import JsonRender.Actions as Actions\n"
    in
    "module "
        ++ namespace
        ++ ".Actions exposing ("
        ++ String.join ", " exposingItems
        ++ ")\n\n"
        ++ imports
        ++ "\n\n"
        ++ enumSection
        ++ paramsTypesStr
        ++ actionType actions
        ++ "\n\n"
        ++ decodeActionFunction actions
        ++ "\n\n\n"
        ++ handleActionFunction
        ++ "\n\n"
        ++ actionConfigFunction


functionsModule : String -> Dict String SchemaParser.FunctionSchema -> String
functionsModule namespace functions =
    let
        sortedFunctions =
            Dict.toList functions |> List.sortBy Tuple.first

        enums =
            List.concatMap (\( _, schema ) -> collectEnumsFromFields schema.params) sortedFunctions
                |> dedupLists

        enumExposing =
            List.map (\variants -> TypeMapping.toElmType (FEnum variants) ++ "(..)") enums

        exposingItems =
            enumExposing
                ++ [ "Functions"
                   , "functions"
                   , "toFunctionDict"
                   ]

        paramsTypes =
            List.map
                (\( name, schema ) ->
                    functionParamsType (TypeMapping.capitalizeFirst name) schema
                )
                sortedFunctions

        functionsType =
            functionRecordType sortedFunctions

        functionsValue =
            functionRecordValue sortedFunctions

        toFunctionDictCode =
            toFunctionDict sortedFunctions

        enumSection =
            case enums of
                [] ->
                    ""

                _ ->
                    List.map enumHelpers enums
                        |> String.join "\n\n\n"
                        |> (\s -> s ++ "\n\n\n")
    in
    "module "
        ++ namespace
        ++ ".Functions exposing ("
        ++ String.join ", " exposingItems
        ++ ")\n\n"
        ++ "import Dict exposing (Dict)\n"
        ++ "import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue(..))\n\n\n"
        ++ enumSection
        ++ String.join "\n\n\n" paramsTypes
        ++ "\n\n\n"
        ++ functionsType
        ++ "\n\n\n"
        ++ functionsValue
        ++ "\n\n\n"
        ++ toFunctionDictCode
        ++ "\n"


functionParamsType : String -> SchemaParser.FunctionSchema -> String
functionParamsType capitalName schema =
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
    "type alias " ++ capitalName ++ "Params =\n" ++ body


functionRecordType : List ( String, SchemaParser.FunctionSchema ) -> String
functionRecordType functions =
    let
        fields =
            List.map
                (\( name, schema ) ->
                    let
                        capitalName =
                            TypeMapping.capitalizeFirst name

                        returnType =
                            TypeMapping.toElmType schema.returnType
                    in
                    name ++ " : " ++ capitalName ++ "Params -> " ++ returnType
                )
                functions

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
    "type alias Functions =\n" ++ body


functionRecordValue : List ( String, SchemaParser.FunctionSchema ) -> String
functionRecordValue functions =
    let
        fields =
            List.map (\( name, _ ) -> name ++ " = ()") functions

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
    "functions : Functions\nfunctions =\n" ++ body


toFunctionDict : List ( String, SchemaParser.FunctionSchema ) -> String
toFunctionDict functions =
    let
        entries =
            List.map toFunctionDictEntry functions

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
    "toFunctionDict : Functions -> Dict String (Dict String ResolvedValue -> ResolvedValue)\n"
        ++ "toFunctionDict fns =\n"
        ++ "    Dict.fromList\n"
        ++ entriesStr


toFunctionDictEntry : ( String, SchemaParser.FunctionSchema ) -> String
toFunctionDictEntry ( name, schema ) =
    let
        capitalName =
            TypeMapping.capitalizeFirst name

        sortedParams =
            Dict.toList schema.params
                |> List.sortBy Tuple.first

        pipelineSteps =
            List.map
                (\( pName, field ) ->
                    let
                        extractor =
                            TypeMapping.toResolvedValueExtractor field.fieldType
                    in
                    if field.required then
                        "                        |> ResolvedValue.required \"" ++ pName ++ "\" " ++ extractor

                    else
                        "                        |> ResolvedValue.optional \"" ++ pName ++ "\" " ++ extractor ++ " Nothing"
                )
                sortedParams

        wrapper =
            TypeMapping.toResolvedValueWrapper schema.returnType
    in
    "( \""
        ++ name
        ++ "\"\n"
        ++ "          , \\args ->\n"
        ++ "                let\n"
        ++ "                    result =\n"
        ++ "                        ResolvedValue.succeed "
        ++ capitalName
        ++ "Params\n"
        ++ String.join "\n" pipelineSteps
        ++ "\n"
        ++ "                in\n"
        ++ "                case result args of\n"
        ++ "                    Ok params ->\n"
        ++ "                        "
        ++ wrapper
        ++ " (fns."
        ++ name
        ++ " params)\n\n"
        ++ "                    Err err ->\n"
        ++ "                        RError (\""
        ++ name
        ++ ": \" ++ err)\n"
        ++ "          )"


registryModule : String -> List String -> Bool -> String
registryModule namespace componentNames hasFunctions =
    let
        imports =
            List.map
                (\name -> "import " ++ namespace ++ ".Components." ++ name)
                componentNames

        functionsImport =
            if hasFunctions then
                "import " ++ namespace ++ ".Functions\n"

            else
                ""

        entries =
            List.map
                (\name ->
                    "        ( \""
                        ++ name
                        ++ "\", "
                        ++ namespace
                        ++ ".Components."
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

        functionsField =
            if hasFunctions then
                "    , functions = " ++ namespace ++ ".Functions.toFunctionDict " ++ namespace ++ ".Functions.functions\n"

            else
                "    , functions = Dict.empty\n"
    in
    "module "
        ++ namespace
        ++ ".Registry exposing (registry)\n\n"
        ++ "import Dict\n"
        ++ "import JsonRender.Render exposing (Registry)\n"
        ++ functionsImport
        ++ String.join "\n" imports
        ++ "\n\n\n"
        ++ "registry : Registry msg\nregistry =\n    { components =\n        Dict.fromList\n"
        ++ entriesStr
        ++ "\n"
        ++ functionsField
        ++ "    }\n"
