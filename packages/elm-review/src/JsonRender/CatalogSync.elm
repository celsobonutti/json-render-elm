module JsonRender.CatalogSync exposing (Config, findDeclarationRange, findManagedCommentRange, rule)

{-| elm-review rule that keeps Elm component modules in sync with a json-render catalog.

Workflow:

1.  Run `elm-review` — reports which component files to create
2.  Create empty stubs: `module Components.Card exposing (..)`
3.  Run `elm-review --fix` — fills stubs with correct types, decoders, view placeholder
4.  Implement the view functions (replace the `()` placeholders)

-}

import Dict
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Json.Decode as Decode
import JsonRender.Internal.ElmCodeGen as ElmCodeGen
import JsonRender.Internal.SchemaParser as SchemaParser exposing (CatalogSchema)
import JsonRender.Internal.TypeMapping as TypeMapping
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


type alias Config =
    { schemaJson : String
    , catalogNamespace : String
    }


type alias ProjectContext =
    { catalog : Maybe CatalogSchema
    , config : Config
    , seenModules : Set String
    , registrySeen : Bool
    , registryComponents : Set String
    , actionsSeen : Bool
    , functionsSeen : Bool
    }


type alias ModuleContext =
    { catalog : Maybe CatalogSchema
    , config : Config
    , moduleName : String
    , isComponentModule : Maybe String
    , isRegistryModule : Bool
    , isActionsModule : Bool
    , isFunctionsModule : Bool
    , registryEntries : Set String
    , moduleKey : Rule.ModuleKey
    , moduleRange : Maybe Range
    , hasPropsDecoder : Bool
    , extractSourceCode : Range -> String
    , importedModules : Set String
    , lastImportEnd : Maybe { row : Int, column : Int }
    }


rule : Config -> Rule
rule config =
    let
        catalog =
            Decode.decodeString SchemaParser.decoder config.schemaJson
                |> Result.toMaybe
    in
    Rule.newProjectRuleSchema "JsonRender.CatalogSync" (initialProjectContext catalog config)
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule catalog config
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation (finalEvaluation config)
        |> Rule.fromProjectRuleSchema


initialProjectContext : Maybe CatalogSchema -> Config -> ProjectContext
initialProjectContext catalog config =
    { catalog = catalog
    , config = config
    , seenModules = Set.empty
    , registrySeen = False
    , registryComponents = Set.empty
    , actionsSeen = False
    , functionsSeen = False
    }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor


fromProjectToModule : Maybe CatalogSchema -> Config -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule catalog config =
    Rule.initContextCreator
        (\moduleKey moduleName extractSourceCode _ ->
            let
                moduleStr =
                    String.join "." (Node.value moduleName)

                componentsPrefix =
                    config.catalogNamespace ++ ".Components."

                componentName =
                    case catalog of
                        Just cat ->
                            if String.startsWith componentsPrefix moduleStr then
                                let
                                    suffix =
                                        String.dropLeft (String.length componentsPrefix) moduleStr
                                in
                                if Dict.member suffix cat.components then
                                    Just suffix

                                else
                                    Nothing

                            else
                                Nothing

                        Nothing ->
                            Nothing
            in
            { catalog = catalog
            , config = config
            , moduleName = moduleStr
            , isComponentModule = componentName
            , isRegistryModule = moduleStr == config.catalogNamespace ++ ".Registry"
            , isActionsModule = moduleStr == config.catalogNamespace ++ ".Actions"
            , isFunctionsModule = moduleStr == config.catalogNamespace ++ ".Functions"
            , registryEntries = Set.empty
            , moduleKey = moduleKey
            , moduleRange = Nothing
            , hasPropsDecoder = False
            , extractSourceCode = extractSourceCode
            , importedModules = Set.empty
            , lastImportEnd = Nothing
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleNameNode
        |> Rule.withSourceCodeExtractor


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\moduleCtx ->
            { catalog = moduleCtx.catalog
            , config = moduleCtx.config
            , seenModules =
                case moduleCtx.isComponentModule of
                    Just name ->
                        Set.singleton name

                    Nothing ->
                        Set.empty
            , registrySeen = moduleCtx.isRegistryModule
            , registryComponents = moduleCtx.registryEntries
            , actionsSeen = moduleCtx.isActionsModule
            , functionsSeen = moduleCtx.isFunctionsModule
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts a b =
    { catalog = a.catalog
    , config = a.config
    , seenModules = Set.union a.seenModules b.seenModules
    , registrySeen = a.registrySeen || b.registrySeen
    , registryComponents = Set.union a.registryComponents b.registryComponents
    , actionsSeen = a.actionsSeen || b.actionsSeen
    , functionsSeen = a.functionsSeen || b.functionsSeen
    }


moduleDefinitionVisitor : Node Module.Module -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
moduleDefinitionVisitor (Node range _) context =
    ( [], { context | moduleRange = Just range } )


importVisitor : Node Import -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
importVisitor (Node range import_) context =
    let
        moduleName =
            String.join "." (Node.value import_.moduleName)
    in
    ( []
    , { context
        | importedModules = Set.insert moduleName context.importedModules
        , lastImportEnd = Just range.end
      }
    )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor declarations context =
    case context.catalog of
        Nothing ->
            ( [], context )

        Just catalog ->
            if context.isRegistryModule then
                let
                    entries =
                        extractRegistryEntries declarations

                    catalogNames =
                        Dict.keys catalog.components |> Set.fromList

                    missing =
                        Set.diff catalogNames entries

                    errors =
                        if Set.isEmpty missing then
                            []

                        else
                            case context.moduleRange of
                                Just range ->
                                    let
                                        fullRange =
                                            { start = range.start
                                            , end = lastDeclarationEnd declarations range
                                            }

                                        generatedCode =
                                            ElmCodeGen.registryModule
                                                context.config.catalogNamespace
                                                (Dict.keys catalog.components |> List.sort)
                                                (not (Dict.isEmpty catalog.functions))
                                    in
                                    [ Rule.errorWithFix
                                        { message = "Registry is missing components: " ++ String.join ", " (Set.toList missing)
                                        , details =
                                            [ "The registry module does not include all catalog components."
                                            , "Accept the fix to regenerate it."
                                            ]
                                        }
                                        range
                                        [ Fix.replaceRangeBy fullRange generatedCode ]
                                    ]

                                Nothing ->
                                    []
                in
                ( errors, { context | registryEntries = entries } )

            else if context.isActionsModule then
                let
                    existingVariants =
                        extractActionVariants declarations

                    expectedVariants =
                        Dict.keys catalog.actions
                            |> List.map TypeMapping.capitalizeFirst
                            |> Set.fromList

                    missingVariants =
                        Set.diff expectedVariants existingVariants

                    hasActionConfig =
                        List.any (hasFunction "actionConfig") declarations

                    hasHandleAction =
                        List.any (hasFunction "handleAction") declarations

                    needsFix =
                        not (Set.isEmpty missingVariants) || not hasActionConfig || not hasHandleAction

                    errorMessage =
                        let
                            missingParts =
                                (if not (Set.isEmpty missingVariants) then
                                    [ "variants: " ++ String.join ", " (Set.toList missingVariants) ]

                                 else
                                    []
                                )
                                    ++ (if not hasActionConfig then
                                            [ "actionConfig" ]

                                        else
                                            []
                                       )
                                    ++ (if not hasHandleAction then
                                            [ "handleAction" ]

                                        else
                                            []
                                       )
                        in
                        "Actions module is missing " ++ String.join ", " missingParts

                    errors =
                        if not needsFix then
                            []

                        else
                            case context.moduleRange of
                                Just range ->
                                    let
                                        fullRange =
                                            { start = range.start
                                            , end = lastDeclarationEnd declarations range
                                            }

                                        generatedCode =
                                            ElmCodeGen.actionsModule
                                                context.config.catalogNamespace
                                                catalog.actions
                                    in
                                    [ Rule.errorWithFix
                                        { message = errorMessage
                                        , details =
                                            [ "The Actions module does not match the catalog actions."
                                            , "Accept the fix to regenerate it."
                                            ]
                                        }
                                        range
                                        [ Fix.replaceRangeBy fullRange generatedCode ]
                                    ]

                                Nothing ->
                                    []
                in
                ( errors, context )

            else if context.isFunctionsModule then
                let
                    hasToFunctionDict =
                        List.any (hasFunction "toFunctionDict") declarations

                    errors =
                        if hasToFunctionDict then
                            []

                        else
                            case context.moduleRange of
                                Just range ->
                                    let
                                        fullRange =
                                            { start = range.start
                                            , end = lastDeclarationEnd declarations range
                                            }

                                        generatedCode =
                                            ElmCodeGen.functionsModule
                                                context.config.catalogNamespace
                                                catalog.functions
                                    in
                                    [ Rule.errorWithFix
                                        { message = "Functions module is missing toFunctionDict"
                                        , details =
                                            [ "This module should contain function types and a toFunctionDict converter."
                                            , "Accept the fix to generate the correct types and converter."
                                            ]
                                        }
                                        range
                                        [ Fix.replaceRangeBy fullRange generatedCode ]
                                    ]

                                Nothing ->
                                    []
                in
                ( errors, context )

            else
                case context.isComponentModule of
                    Just componentName ->
                        let
                            errors =
                                case ( context.moduleRange, Dict.get componentName catalog.components ) of
                                    ( Just range, Just schema ) ->
                                        let
                                            viewRange =
                                                findFunctionRange "view" declarations
                                        in
                                        case viewRange of
                                            Nothing ->
                                                -- No view function: generate full module (new stub)
                                                let
                                                    fullRange =
                                                        { start = range.start
                                                        , end = lastDeclarationEnd declarations range
                                                        }
                                                in
                                                [ Rule.errorWithFix
                                                    { message = componentName ++ " component is missing view function"
                                                    , details =
                                                        [ "This module should contain types, decoders, and a view function matching the catalog."
                                                        , "Accept the fix to generate the correct code with a view placeholder."
                                                        ]
                                                    }
                                                    range
                                                    [ Fix.replaceRangeBy fullRange
                                                        (ElmCodeGen.componentModule
                                                            context.config.catalogNamespace
                                                            componentName
                                                            schema
                                                        )
                                                    ]
                                                ]

                                            Just vRange ->
                                                let
                                                    expected =
                                                        ElmCodeGen.expectedDeclarations componentName schema

                                                    expectedComment =
                                                        ElmCodeGen.generatedComment componentName schema

                                                    -- Check each declaration
                                                    declFixes =
                                                        List.concatMap
                                                            (\decl ->
                                                                case findDeclarationRange decl.name decl.kind declarations of
                                                                    Just declRange ->
                                                                        let
                                                                            currentCode =
                                                                                context.extractSourceCode declRange
                                                                        in
                                                                        if String.trim currentCode == String.trim decl.code then
                                                                            []

                                                                        else
                                                                            [ Fix.replaceRangeBy declRange (decl.code ++ "\n") ]

                                                                    Nothing ->
                                                                        [ Fix.insertAt vRange.start (decl.code ++ "\n\n\n") ]
                                                            )
                                                            expected

                                                    -- Check managed comment
                                                    commentFixes =
                                                        case findManagedCommentRange context.extractSourceCode of
                                                            Just commentRange ->
                                                                let
                                                                    currentComment =
                                                                        context.extractSourceCode commentRange
                                                                in
                                                                if String.trim currentComment == String.trim expectedComment then
                                                                    []

                                                                else
                                                                    [ Fix.replaceRangeBy commentRange (expectedComment ++ "\n") ]

                                                            Nothing ->
                                                                -- No comment found, insert at very top
                                                                [ Fix.insertAt { row = 1, column = 1 } (expectedComment ++ "\n") ]

                                                    -- Check imports
                                                    missingImportLines =
                                                        requiredComponentImportLines
                                                            |> List.filter (\( modName, _ ) -> not (Set.member modName context.importedModules))

                                                    importFixes =
                                                        case ( missingImportLines, context.lastImportEnd ) of
                                                            ( _ :: _, Just insertPos ) ->
                                                                List.map
                                                                    (\( _, line ) -> Fix.insertAt insertPos ("\n" ++ line))
                                                                    missingImportLines

                                                            _ ->
                                                                []

                                                    allFixes =
                                                        commentFixes ++ importFixes ++ declFixes
                                                in
                                                if List.isEmpty allFixes then
                                                    []

                                                else
                                                    [ Rule.errorWithFix
                                                        { message = componentName ++ " component types are out of sync with the catalog"
                                                        , details =
                                                            [ "The generated types and decoders in this module don't match the catalog schema."
                                                            , "Accept the fix to regenerate them. Your view function will be preserved."
                                                            ]
                                                        }
                                                        range
                                                        allFixes
                                                    ]

                                    _ ->
                                        []
                        in
                        ( errors, context )

                    Nothing ->
                        ( [], context )


requiredComponentImportLines : List ( String, String )
requiredComponentImportLines =
    [ ( "Dict", "import Dict exposing (Dict)" )
    , ( "Json.Encode", "import Json.Encode exposing (Value)" )
    , ( "JsonRender.Bind", "import JsonRender.Bind as Bind" )
    , ( "JsonRender.Events", "import JsonRender.Events exposing (EventHandle)" )
    , ( "JsonRender.Render", "import JsonRender.Render exposing (Component, ComponentContext, register)" )
    , ( "JsonRender.Resolve", "import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)" )
    ]


findFunctionRange : String -> List (Node Declaration) -> Maybe Range
findFunctionRange name declarations =
    List.filterMap
        (\(Node range decl) ->
            case decl of
                Declaration.FunctionDeclaration func ->
                    if Node.value (Node.value func.declaration).name == name then
                        Just range

                    else
                        Nothing

                _ ->
                    Nothing
        )
        declarations
        |> List.head


findDeclarationRange : String -> ElmCodeGen.DeclKind -> List (Node Declaration) -> Maybe Range
findDeclarationRange name kind declarations =
    List.filterMap
        (\(Node range decl) ->
            case ( kind, decl ) of
                ( ElmCodeGen.FunctionDecl, Declaration.FunctionDeclaration func ) ->
                    if Node.value (Node.value func.declaration).name == name then
                        Just range

                    else
                        Nothing

                ( ElmCodeGen.TypeAliasDecl, Declaration.AliasDeclaration typeAlias ) ->
                    if Node.value typeAlias.name == name then
                        Just range

                    else
                        Nothing

                ( ElmCodeGen.CustomTypeDecl, Declaration.CustomTypeDeclaration typeDecl ) ->
                    if Node.value typeDecl.name == name then
                        Just range

                    else
                        Nothing

                _ ->
                    Nothing
        )
        declarations
        |> List.head


findManagedCommentRange : (Range -> String) -> Maybe Range
findManagedCommentRange extractSourceCode =
    let
        -- Read from the very beginning of the file
        startRange =
            { start = { row = 1, column = 1 }
            , end = { row = 1, column = 2 }
            }

        firstChar =
            extractSourceCode startRange
    in
    if firstChar == "{" then
        -- There might be a comment. Find the end of it by searching for "-}"
        let
            chunk =
                extractSourceCode
                    { start = { row = 1, column = 1 }
                    , end = { row = 50, column = 1 }
                    }

            closingIndex =
                String.indexes "-}" chunk |> List.head
        in
        case closingIndex of
            Just idx ->
                let
                    commentText =
                        String.left (idx + 2) chunk
                in
                if String.startsWith "{- This module was generated by the CatalogSync elm-review rule." commentText then
                    let
                        lines =
                            String.lines commentText

                        lineCount =
                            List.length lines

                        lastLine =
                            List.reverse lines |> List.head |> Maybe.withDefault ""
                    in
                    Just
                        { start = { row = 1, column = 1 }
                        , end = { row = lineCount, column = String.length lastLine + 1 }
                        }

                else
                    Nothing

            Nothing ->
                Nothing

    else
        Nothing


hasFunction : String -> Node Declaration -> Bool
hasFunction name (Node _ decl) =
    case decl of
        Declaration.FunctionDeclaration func ->
            Node.value (Node.value func.declaration).name == name

        _ ->
            False


lastDeclarationEnd : List (Node Declaration) -> Range -> { row : Int, column : Int }
lastDeclarationEnd declarations moduleRange =
    case List.reverse declarations of
        (Node range _) :: _ ->
            range.end

        [] ->
            moduleRange.end


extractRegistryEntries : List (Node Declaration) -> Set String
extractRegistryEntries declarations =
    List.foldl
        (\(Node _ decl) acc ->
            case decl of
                Declaration.FunctionDeclaration func ->
                    let
                        implementation =
                            Node.value func.declaration

                        name =
                            Node.value implementation.name
                    in
                    if name == "registry" then
                        extractEntriesFromExpression (Node.value implementation.expression) acc

                    else
                        acc

                _ ->
                    acc
        )
        Set.empty
        declarations


extractActionVariants : List (Node Declaration) -> Set String
extractActionVariants declarations =
    List.foldl
        (\(Node _ decl) acc ->
            case decl of
                Declaration.CustomTypeDeclaration typeDecl ->
                    if Node.value typeDecl.name == "Action" then
                        List.foldl
                            (\(Node _ constructor) innerAcc ->
                                Set.insert (Node.value constructor.name) innerAcc
                            )
                            acc
                            typeDecl.constructors

                    else
                        acc

                _ ->
                    acc
        )
        Set.empty
        declarations


extractEntriesFromExpression : Expression -> Set String -> Set String
extractEntriesFromExpression expr acc =
    case expr of
        Expression.Application nodes ->
            case nodes of
                (Node _ (Expression.FunctionOrValue [ "Dict" ] "fromList")) :: (Node _ listExpr) :: _ ->
                    extractEntriesFromExpression listExpr acc

                _ ->
                    acc

        Expression.ListExpr items ->
            List.foldl
                (\(Node _ item) innerAcc ->
                    case item of
                        Expression.TupledExpression ((Node _ (Expression.Literal componentName)) :: _) ->
                            Set.insert componentName innerAcc

                        _ ->
                            innerAcc
                )
                acc
                items

        Expression.RecordExpr fields ->
            List.foldl
                (\(Node _ ( Node _ fieldName, Node _ fieldExpr )) innerAcc ->
                    if fieldName == "components" then
                        extractEntriesFromExpression fieldExpr innerAcc

                    else
                        innerAcc
                )
                acc
                fields

        _ ->
            acc


finalEvaluation : Config -> ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluation config projectCtx =
    case projectCtx.catalog of
        Nothing ->
            []

        Just catalog ->
            let
                nsPath =
                    String.replace "." "/" config.catalogNamespace

                missingComponents =
                    Dict.keys catalog.components
                        |> List.filter (\name -> not (Set.member name projectCtx.seenModules))
                        |> List.sort

                missingInfra =
                    (if not projectCtx.registrySeen then
                        [ "Registry" ]

                     else
                        []
                    )
                        ++ (if not projectCtx.actionsSeen && not (Dict.isEmpty catalog.actions) then
                                [ "Actions" ]

                            else
                                []
                           )
                        ++ (if not projectCtx.functionsSeen && not (Dict.isEmpty catalog.functions) then
                                [ "Functions" ]

                            else
                                []
                           )

                allMissing =
                    List.map (\name -> { name = name, isComponent = True }) missingComponents
                        ++ List.map (\name -> { name = name, isComponent = False }) missingInfra
            in
            if List.isEmpty allMissing then
                []

            else
                let
                    moduleAndPath entry =
                        if entry.isComponent then
                            { moduleName = config.catalogNamespace ++ ".Components." ++ entry.name
                            , filePath = "src/" ++ nsPath ++ "/Components/" ++ entry.name ++ ".elm"
                            }

                        else
                            { moduleName = config.catalogNamespace ++ "." ++ entry.name
                            , filePath = "src/" ++ nsPath ++ "/" ++ entry.name ++ ".elm"
                            }

                    dirs =
                        ("mkdir -p src/" ++ nsPath)
                            ++ (if List.any .isComponent allMissing then
                                    " src/" ++ nsPath ++ "/Components"

                                else
                                    ""
                               )

                    stubCommand =
                        allMissing
                            |> List.map
                                (\entry ->
                                    let
                                        info =
                                            moduleAndPath entry
                                    in
                                    "echo 'module "
                                        ++ info.moduleName
                                        ++ " exposing (..)' > "
                                        ++ info.filePath
                                )
                            |> String.join " && \\\n  "

                    fileList =
                        allMissing
                            |> List.map (\entry -> (moduleAndPath entry).filePath)
                            |> String.join ", "
                in
                [ Rule.globalError
                    { message = "Missing modules: " ++ String.join ", " (List.map .name allMissing)
                    , details =
                        [ "Create these files: " ++ fileList
                        , "Quick stub command:\n\n" ++ dirs ++ " && \\\n  " ++ stubCommand
                        , "Then run elm-review --fix to fill them in."
                        ]
                    }
                ]
