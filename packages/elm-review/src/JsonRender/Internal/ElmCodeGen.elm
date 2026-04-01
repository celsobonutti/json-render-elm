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
import Elm.CodeGen as CG
import Elm.Pretty
import JsonRender.Internal.SchemaParser as SchemaParser exposing (ActionSchema, ComponentSchema, FieldType(..))
import JsonRender.Internal.TypeMapping as TypeMapping
import Pretty


renderDecl : CG.Declaration -> String
renderDecl decl =
    Elm.Pretty.prettyDeclaration 120 decl
        |> Pretty.pretty 120


fieldToTypeAnnotation : String -> SchemaParser.FieldType -> CG.TypeAnnotation
fieldToTypeAnnotation fieldName fieldType =
    case fieldType of
        SchemaParser.FString ->
            CG.stringAnn

        SchemaParser.FInt ->
            CG.intAnn

        SchemaParser.FFloat ->
            CG.floatAnn

        SchemaParser.FBool ->
            CG.boolAnn

        SchemaParser.FNullable inner ->
            CG.maybeAnn (fieldToTypeAnnotation fieldName inner)

        SchemaParser.FList inner ->
            CG.listAnn (fieldToTypeAnnotation fieldName inner)

        SchemaParser.FEnum variants ->
            CG.typed (TypeMapping.toElmType (SchemaParser.FEnum variants)) []

        SchemaParser.FObject _ ->
            CG.typed (objectTypeName fieldName) []


schemaFieldToTypeAnnotation : SchemaParser.FieldType -> CG.TypeAnnotation
schemaFieldToTypeAnnotation fieldType =
    case fieldType of
        SchemaParser.FString ->
            CG.stringAnn

        SchemaParser.FInt ->
            CG.intAnn

        SchemaParser.FFloat ->
            CG.floatAnn

        SchemaParser.FBool ->
            CG.boolAnn

        SchemaParser.FNullable inner ->
            CG.maybeAnn (schemaFieldToTypeAnnotation inner)

        SchemaParser.FList inner ->
            CG.listAnn (schemaFieldToTypeAnnotation inner)

        SchemaParser.FEnum variants ->
            CG.typed (TypeMapping.toElmType (SchemaParser.FEnum variants)) []

        SchemaParser.FObject _ ->
            CG.typed "Value" []


recordTypeAlias : String -> List ( String, CG.TypeAnnotation ) -> CG.Declaration
recordTypeAlias name fields =
    CG.aliasDecl Nothing name [] (CG.recordAnn fields)


propsTypeAlias : String -> ComponentSchema -> String
propsTypeAlias componentName schema =
    let
        fields =
            Dict.toList schema.fields
                |> List.sortBy Tuple.first
                |> List.map
                    (\( name, field ) ->
                        ( name
                        , if field.required then
                            fieldToTypeAnnotation name field.fieldType

                          else
                            CG.maybeAnn (fieldToTypeAnnotation name field.fieldType)
                        )
                    )
    in
    recordTypeAlias (componentName ++ "Props") fields
        |> renderDecl


fieldToExtractorExpr : String -> SchemaParser.FieldType -> CG.Expression
fieldToExtractorExpr fieldName fieldType =
    case fieldType of
        SchemaParser.FObject _ ->
            CG.parens
                (CG.lambda [ CG.varPattern "rv" ]
                    (CG.pipe
                        (CG.apply [ CG.fqVal [ "ResolvedValue" ] "object", CG.val "rv" ])
                        [ CG.apply [ CG.fqVal [ "Result" ] "andThen", CG.val (objectFnBaseName fieldName ++ "Decoder") ] ]
                    )
                )

        SchemaParser.FEnum variants ->
            let
                baseName =
                    TypeMapping.enumFnBaseName variants
            in
            CG.parens
                (CG.lambda [ CG.varPattern "rv" ]
                    (CG.pipe
                        (CG.apply [ CG.fqVal [ "ResolvedValue" ] "string", CG.val "rv" ])
                        [ CG.apply [ CG.fqVal [ "Result" ] "andThen", CG.val (baseName ++ "FromString") ] ]
                    )
                )

        _ ->
            CG.fqVal [ "ResolvedValue" ] (schemaFieldExtractorName fieldType)


schemaFieldExtractorName : SchemaParser.FieldType -> String
schemaFieldExtractorName fieldType =
    case fieldType of
        SchemaParser.FString ->
            "string"

        SchemaParser.FInt ->
            "int"

        SchemaParser.FFloat ->
            "float"

        SchemaParser.FBool ->
            "bool"

        _ ->
            "string"


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
                            fieldToExtractorExpr name field.fieldType

                        args =
                            if field.required then
                                [ CG.fqVal [ "ResolvedValue" ] "required", CG.string name, extractor ]

                            else
                                [ CG.fqVal [ "ResolvedValue" ] "optional", CG.string name, extractor, CG.val "Nothing" ]
                    in
                    CG.apply args
                )
                fields

        body =
            CG.pipe
                (CG.apply [ CG.fqVal [ "ResolvedValue" ] "succeed", CG.val (componentName ++ "Props") ])
                pipelineSteps

        typeAnn =
            CG.funAnn
                (CG.typed "Dict" [ CG.stringAnn, CG.typed "ResolvedValue" [] ])
                (CG.typed "Result" [ CG.stringAnn, CG.typed (componentName ++ "Props") [] ])
    in
    CG.funDecl Nothing (Just typeAnn) "propsDecoder" [] body
        |> renderDecl


bindingsTypeAlias : String -> ComponentSchema -> String
bindingsTypeAlias componentName schema =
    let
        fields =
            Dict.toList schema.fields
                |> List.sortBy Tuple.first
                |> List.map
                    (\( name, field ) ->
                        ( name
                        , CG.maybeAnn
                            (CG.funAnn
                                (fieldToTypeAnnotation name field.fieldType)
                                (CG.typed "EventHandle" [ CG.typeVar "msg" ])
                            )
                        )
                    )
    in
    CG.aliasDecl Nothing
        (componentName ++ "Bindings")
        [ "msg" ]
        (CG.recordAnn fields)
        |> renderDecl


fieldToEncoderExpr : String -> SchemaParser.FieldType -> CG.Expression
fieldToEncoderExpr fieldName fieldType =
    case fieldType of
        SchemaParser.FObject _ ->
            CG.val (objectFnBaseName fieldName ++ "Encoder")

        SchemaParser.FEnum variants ->
            CG.parens
                (CG.applyBinOp
                    (CG.fqVal [ "Json", "Encode" ] "string")
                    CG.composel
                    (CG.val (TypeMapping.enumFnBaseName variants ++ "ToString"))
                )

        SchemaParser.FString ->
            CG.fqVal [ "Json", "Encode" ] "string"

        SchemaParser.FInt ->
            CG.fqVal [ "Json", "Encode" ] "int"

        SchemaParser.FFloat ->
            CG.fqVal [ "Json", "Encode" ] "float"

        SchemaParser.FBool ->
            CG.fqVal [ "Json", "Encode" ] "bool"

        SchemaParser.FNullable inner ->
            CG.parens
                (CG.applyBinOp
                    (CG.apply [ CG.fqVal [ "Maybe" ] "map", fieldToEncoderExpr fieldName inner ])
                    CG.composer
                    (CG.apply [ CG.fqVal [ "Maybe" ] "withDefault", CG.fqVal [ "Json", "Encode" ] "null" ])
                )

        SchemaParser.FList inner ->
            CG.parens
                (CG.apply [ CG.fqVal [ "Json", "Encode" ] "list", fieldToEncoderExpr fieldName inner ])


bindingsDecoder : String -> ComponentSchema -> String
bindingsDecoder componentName schema =
    let
        fields =
            Dict.toList schema.fields
                |> List.sortBy Tuple.first

        pipelineSteps =
            List.map
                (\( name, field ) ->
                    CG.apply
                        [ CG.fqVal [ "Bind" ] "bindableTyped"
                        , CG.string name
                        , fieldToEncoderExpr name field.fieldType
                        ]
                )
                fields

        body =
            CG.pipe
                (CG.apply [ CG.fqVal [ "Bind" ] "succeed", CG.val (componentName ++ "Bindings") ])
                pipelineSteps

        typeAnn =
            CG.funAnn
                (CG.typed "Dict" [ CG.stringAnn, CG.funAnn (CG.typed "Value" []) (CG.typed "EventHandle" [ CG.typeVar "msg" ]) ])
                (CG.typed (componentName ++ "Bindings") [ CG.typeVar "msg" ])
    in
    CG.funDecl Nothing (Just typeAnn) "bindingsDecoder" [] body
        |> renderDecl


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
    let
        typeAnn =
            CG.funAnn
                (CG.typed "ComponentContext"
                    [ CG.typed (componentName ++ "Props") []
                    , CG.typed (componentName ++ "Bindings") [ CG.typeVar "msg" ]
                    , CG.typeVar "msg"
                    ]
                )
                (CG.typed "Html" [ CG.typeVar "msg" ])
    in
    CG.funDecl Nothing (Just typeAnn) "view" [ CG.varPattern "ctx" ] CG.unit
        |> renderDecl


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
                        ( name
                        , if field.required then
                            schemaFieldToTypeAnnotation field.fieldType

                          else
                            CG.maybeAnn (schemaFieldToTypeAnnotation field.fieldType)
                        )
                    )
    in
    recordTypeAlias (actionName ++ "Params") fields
        |> renderDecl


actionType : Dict String ActionSchema -> String
actionType actions =
    let
        sortedActions =
            Dict.toList actions
                |> List.sortBy Tuple.first

        variants =
            case sortedActions of
                [] ->
                    [ ( "NoAction", [] ) ]

                _ ->
                    List.map
                        (\( name, schema ) ->
                            let
                                capitalName =
                                    TypeMapping.capitalizeFirst name
                            in
                            if Dict.isEmpty schema.params then
                                ( capitalName, [] )

                            else
                                ( capitalName, [ CG.typed (capitalName ++ "Params") [] ] )
                        )
                        sortedActions
    in
    CG.customTypeDecl Nothing "Action" [] variants
        |> renderDecl


fieldToJsonDecoderExpr : SchemaParser.FieldType -> CG.Expression
fieldToJsonDecoderExpr fieldType =
    case fieldType of
        SchemaParser.FString ->
            CG.fqVal [ "Decode" ] "string"

        SchemaParser.FInt ->
            CG.fqVal [ "Decode" ] "int"

        SchemaParser.FFloat ->
            CG.fqVal [ "Decode" ] "float"

        SchemaParser.FBool ->
            CG.fqVal [ "Decode" ] "bool"

        SchemaParser.FNullable inner ->
            CG.parens (CG.apply [ CG.fqVal [ "Decode" ] "nullable", fieldToJsonDecoderExpr inner ])

        SchemaParser.FList inner ->
            CG.parens (CG.apply [ CG.fqVal [ "Decode" ] "list", fieldToJsonDecoderExpr inner ])

        SchemaParser.FEnum variants ->
            CG.val (TypeMapping.enumFnBaseName variants ++ "Decoder")

        SchemaParser.FObject _ ->
            CG.fqVal [ "Decode" ] "value"


nestedParamDecodingExpr : List ( String, { a | fieldType : SchemaParser.FieldType, required : Bool } ) -> CG.Expression -> CG.Expression
nestedParamDecodingExpr params successExpr =
    case params of
        [] ->
            successExpr

        ( pName, field ) :: rest ->
            let
                decoderExpr =
                    fieldToJsonDecoderExpr field.fieldType

                innerExpr =
                    nestedParamDecodingExpr rest successExpr
            in
            if field.required then
                -- case Dict.get "paramName" params of
                --     Just paramName_raw ->
                --         case Decode.decodeValue decoder paramName_raw of
                --             Ok paramName -> <recurse>
                --             Err _ -> Err "paramName must be a Type"
                --     Nothing ->
                --         Err "missing required param paramName"
                CG.caseExpr
                    (CG.apply [ CG.fqVal [ "Dict" ] "get", CG.string pName, CG.val "params" ])
                    [ ( CG.namedPattern "Just" [ CG.varPattern (pName ++ "_raw") ]
                      , CG.caseExpr
                            (CG.apply [ CG.fqVal [ "Decode" ] "decodeValue", decoderExpr, CG.val (pName ++ "_raw") ])
                            [ ( CG.namedPattern "Ok" [ CG.varPattern pName ]
                              , innerExpr
                              )
                            , ( CG.namedPattern "Err" [ CG.allPattern ]
                              , CG.apply [ CG.val "Err", CG.string (pName ++ " must be a " ++ TypeMapping.toElmType field.fieldType) ]
                              )
                            ]
                      )
                    , ( CG.namedPattern "Nothing" []
                      , CG.apply [ CG.val "Err", CG.string ("missing required param " ++ pName) ]
                      )
                    ]

            else
                -- let
                --     paramName =
                --         Dict.get "paramName" params
                --             |> Maybe.andThen (\raw -> Decode.decodeValue decoder raw |> Result.toMaybe)
                -- in
                -- <recurse>
                CG.letExpr
                    [ CG.letVal pName
                        (CG.pipe
                            (CG.apply [ CG.fqVal [ "Dict" ] "get", CG.string pName, CG.val "params" ])
                            [ CG.apply
                                [ CG.fqVal [ "Maybe" ] "andThen"
                                , CG.lambda [ CG.varPattern "raw" ]
                                    (CG.pipe
                                        (CG.apply [ CG.fqVal [ "Decode" ] "decodeValue", decoderExpr, CG.val "raw" ])
                                        [ CG.fqVal [ "Result" ] "toMaybe" ]
                                    )
                                ]
                            ]
                        )
                    ]
                    innerExpr


actionBranchExpr : ( String, ActionSchema ) -> ( CG.Pattern, CG.Expression )
actionBranchExpr ( name, schema ) =
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
        ( CG.stringPattern name
        , CG.apply [ CG.val "Ok", CG.val capitalName ]
        )

    else
        let
            allParams =
                sortedParams ++ optionalParams

            recordFields =
                List.map (\( pName, _ ) -> ( pName, CG.val pName )) allParams

            successExpr =
                CG.apply [ CG.val "Ok", CG.parens (CG.apply [ CG.val capitalName, CG.record recordFields ]) ]
        in
        ( CG.stringPattern name
        , nestedParamDecodingExpr allParams successExpr
        )


decodeActionFunction : Dict String ActionSchema -> String
decodeActionFunction actions =
    let
        sortedActions =
            Dict.toList actions
                |> List.sortBy Tuple.first

        actionBranches =
            List.map actionBranchExpr sortedActions

        catchAll =
            ( CG.allPattern
            , CG.apply [ CG.val "Err", CG.parens (CG.applyBinOp (CG.string "Unknown action: ") CG.append (CG.val "name")) ]
            )

        allBranches =
            actionBranches ++ [ catchAll ]

        body =
            CG.caseExpr (CG.val "name") allBranches

        typeAnn =
            CG.funAnn CG.stringAnn
                (CG.funAnn
                    (CG.typed "Dict" [ CG.stringAnn, CG.typed "Value" [] ])
                    (CG.typed "Result" [ CG.stringAnn, CG.typed "Action" [] ])
                )
    in
    CG.funDecl Nothing (Just typeAnn) "decodeAction" [ CG.varPattern "name", CG.varPattern "params" ] body
        |> renderDecl


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
    let
        typeAnn =
            CG.funAnn (CG.typed "Action" [])
                (CG.funAnn
                    (CG.fqTyped [ "Actions" ] "Model" [])
                    (CG.tupleAnn
                        [ CG.fqTyped [ "Actions" ] "Model" []
                        , CG.typed "Cmd" [ CG.fqTyped [ "Actions" ] "Msg" [ CG.typed "Action" [] ] ]
                        ]
                    )
                )
    in
    CG.funDecl Nothing (Just typeAnn) "handleAction" [ CG.varPattern "action", CG.varPattern "model" ] CG.unit
        |> renderDecl


actionConfigFunction : String
actionConfigFunction =
    let
        typeAnn =
            CG.fqTyped [ "Actions" ] "ActionConfig" [ CG.typed "Action" [] ]

        body =
            CG.record
                [ ( "handleAction", CG.val "handleAction" )
                , ( "decodeAction", CG.val "decodeAction" )
                ]
    in
    CG.funDecl Nothing (Just typeAnn) "actionConfig" [] body
        |> renderDecl


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
