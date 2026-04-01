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
        typeAliasCode =
            recordTypeAlias typeName
                (List.map
                    (\( name, field ) ->
                        ( name
                        , if field.required then
                            fieldToTypeAnnotation name field.fieldType

                          else
                            CG.maybeAnn (fieldToTypeAnnotation name field.fieldType)
                        )
                    )
                    sortedFields
                )
                |> renderDecl

        -- decoder (ResolvedValue pipeline)
        decoderPipelineSteps =
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
                sortedFields

        decoderBody =
            CG.pipe
                (CG.apply [ CG.fqVal [ "ResolvedValue" ] "succeed", CG.val typeName ])
                decoderPipelineSteps

        decoderTypeAnn =
            CG.funAnn
                (CG.typed "Dict" [ CG.stringAnn, CG.typed "ResolvedValue" [] ])
                (CG.typed "Result" [ CG.stringAnn, CG.typed typeName [] ])

        decoderCode =
            CG.funDecl Nothing (Just decoderTypeAnn) (baseName ++ "Decoder") [] decoderBody
                |> renderDecl

        -- encoder
        encoderFieldExprs =
            List.map
                (\( name, field ) ->
                    let
                        encoder =
                            fieldToEncoderExpr name field.fieldType

                        valueExpr =
                            if field.required then
                                CG.apply [ encoder, CG.access (CG.val "record") name ]

                            else
                                CG.pipe
                                    (CG.access (CG.val "record") name)
                                    [ CG.apply [ CG.fqVal [ "Maybe" ] "map", encoder ]
                                    , CG.apply [ CG.fqVal [ "Maybe" ] "withDefault", CG.fqVal [ "Json", "Encode" ] "null" ]
                                    ]
                    in
                    CG.tuple [ CG.string name, valueExpr ]
                )
                sortedFields

        encoderBody =
            CG.apply [ CG.fqVal [ "Json", "Encode" ] "object", CG.list encoderFieldExprs ]

        encoderTypeAnn =
            CG.funAnn (CG.typed typeName []) (CG.typed "Value" [])

        encoderCode =
            CG.funDecl Nothing (Just encoderTypeAnn) (baseName ++ "Encoder") [ CG.varPattern "record" ] encoderBody
                |> renderDecl
    in
    typeAliasCode ++ "\n\n\n" ++ decoderCode ++ "\n\n\n" ++ encoderCode


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
                        ( name
                        , if field.required then
                            schemaFieldToTypeAnnotation field.fieldType

                          else
                            CG.maybeAnn (schemaFieldToTypeAnnotation field.fieldType)
                        )
                    )
    in
    recordTypeAlias (capitalName ++ "Params") fields
        |> renderDecl


functionRecordType : List ( String, SchemaParser.FunctionSchema ) -> String
functionRecordType functions =
    let
        fields =
            List.map
                (\( name, schema ) ->
                    ( name
                    , CG.funAnn
                        (CG.typed (TypeMapping.capitalizeFirst name ++ "Params") [])
                        (schemaFieldToTypeAnnotation schema.returnType)
                    )
                )
                functions
    in
    recordTypeAlias "Functions" fields
        |> renderDecl


functionRecordValue : List ( String, SchemaParser.FunctionSchema ) -> String
functionRecordValue functions =
    let
        fields =
            List.map (\( name, _ ) -> ( name, CG.unit )) functions

        body =
            CG.record fields

        typeAnn =
            CG.typed "Functions" []
    in
    CG.funDecl Nothing (Just typeAnn) "functions" [] body
        |> renderDecl


schemaFieldExtractorExpr : SchemaParser.FieldType -> CG.Expression
schemaFieldExtractorExpr fieldType =
    case fieldType of
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


toFunctionDict : List ( String, SchemaParser.FunctionSchema ) -> String
toFunctionDict functions =
    let
        entries =
            List.map toFunctionDictEntry functions

        body =
            CG.apply [ CG.fqVal [ "Dict" ] "fromList", CG.list entries ]

        typeAnn =
            CG.funAnn
                (CG.typed "Functions" [])
                (CG.typed "Dict"
                    [ CG.stringAnn
                    , CG.funAnn
                        (CG.typed "Dict" [ CG.stringAnn, CG.typed "ResolvedValue" [] ])
                        (CG.typed "ResolvedValue" [])
                    ]
                )
    in
    CG.funDecl Nothing (Just typeAnn) "toFunctionDict" [ CG.varPattern "fns" ] body
        |> renderDecl


toFunctionDictEntry : ( String, SchemaParser.FunctionSchema ) -> CG.Expression
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
                            schemaFieldExtractorExpr field.fieldType

                        args =
                            if field.required then
                                [ CG.fqVal [ "ResolvedValue" ] "required", CG.string pName, extractor ]

                            else
                                [ CG.fqVal [ "ResolvedValue" ] "optional", CG.string pName, extractor, CG.val "Nothing" ]
                    in
                    CG.apply args
                )
                sortedParams

        resultExpr =
            CG.pipe
                (CG.apply [ CG.fqVal [ "ResolvedValue" ] "succeed", CG.val (capitalName ++ "Params") ])
                pipelineSteps

        wrapper =
            TypeMapping.toResolvedValueWrapper schema.returnType

        lambdaBody =
            CG.letExpr
                [ CG.letVal "result" resultExpr ]
                (CG.caseExpr
                    (CG.apply [ CG.val "result", CG.val "args" ])
                    [ ( CG.namedPattern "Ok" [ CG.varPattern "params" ]
                      , CG.apply [ CG.val wrapper, CG.parens (CG.apply [ CG.access (CG.val "fns") name, CG.val "params" ]) ]
                      )
                    , ( CG.namedPattern "Err" [ CG.varPattern "err" ]
                      , CG.apply [ CG.val "RError", CG.parens (CG.applyBinOp (CG.string (name ++ ": ")) CG.append (CG.val "err")) ]
                      )
                    ]
                )
    in
    CG.tuple [ CG.string name, CG.lambda [ CG.varPattern "args" ] lambdaBody ]


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
