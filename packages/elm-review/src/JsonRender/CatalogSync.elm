module JsonRender.CatalogSync exposing (Config, rule)

{-| elm-review rule that keeps Elm component modules in sync with a json-render catalog.

Workflow:

1.  Run `elm-review` — reports which component files to create
2.  Create empty stubs: `module Components.Card exposing (..)`
3.  Run `elm-review --fix` — fills stubs with correct types, decoders, view placeholder
4.  Implement the view functions (replace the `()` placeholders)

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type as Type
import Elm.Syntax.Range exposing (Range)
import Json.Decode as Decode
import JsonRender.Internal.ElmCodeGen as ElmCodeGen
import JsonRender.Internal.SchemaParser as SchemaParser exposing (CatalogSchema, ComponentSchema)
import JsonRender.Internal.TypeMapping as TypeMapping
import Review.Fix as Fix
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)


type alias Config =
    { schemaJson : String
    , componentsNamespace : String
    }


type alias ProjectContext =
    { catalog : Maybe CatalogSchema
    , config : Config
    , seenModules : Set String
    , registrySeen : Bool
    , registryComponents : Set String
    , actionsSeen : Bool
    }


type alias ModuleContext =
    { catalog : Maybe CatalogSchema
    , config : Config
    , moduleName : String
    , isComponentModule : Maybe String
    , isRegistryModule : Bool
    , isActionsModule : Bool
    , registryEntries : Set String
    , moduleKey : Rule.ModuleKey
    , moduleRange : Maybe Range
    , hasPropsDecoder : Bool
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
    }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor


fromProjectToModule : Maybe CatalogSchema -> Config -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule catalog config =
    Rule.initContextCreator
        (\moduleKey moduleName _ ->
            let
                moduleStr =
                    String.join "." (Node.value moduleName)

                componentName =
                    case catalog of
                        Just cat ->
                            if String.startsWith (config.componentsNamespace ++ ".") moduleStr then
                                let
                                    suffix =
                                        String.dropLeft (String.length config.componentsNamespace + 1) moduleStr
                                in
                                if suffix == "Registry" then
                                    Nothing

                                else if suffix == "Actions" then
                                    Nothing

                                else if Dict.member suffix cat.components then
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
            , isRegistryModule = moduleStr == config.componentsNamespace ++ ".Registry"
            , isActionsModule = moduleStr == config.componentsNamespace ++ ".Actions"
            , registryEntries = Set.empty
            , moduleKey = moduleKey
            , moduleRange = Nothing
            , hasPropsDecoder = False
            }
        )
        |> Rule.withModuleKey
        |> Rule.withModuleNameNode


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
    }


moduleDefinitionVisitor : Node Module.Module -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
moduleDefinitionVisitor (Node range _) context =
    ( [], { context | moduleRange = Just range } )


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
                                                context.config.componentsNamespace
                                                (Dict.keys catalog.components |> List.sort)
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

                    missing =
                        Set.diff expectedVariants existingVariants

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
                                            ElmCodeGen.actionsModule
                                                context.config.componentsNamespace
                                                catalog.actions
                                    in
                                    [ Rule.errorWithFix
                                        { message = "Actions module is missing variants: " ++ String.join ", " (Set.toList missing)
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

            else
                case context.isComponentModule of
                    Just componentName ->
                        let
                            hasDecoder =
                                List.any (hasFunction "propsDecoder") declarations

                            errors =
                                if hasDecoder then
                                    []

                                else
                                    case ( context.moduleRange, Dict.get componentName catalog.components ) of
                                        ( Just range, Just schema ) ->
                                            let
                                                fullRange =
                                                    { start = range.start
                                                    , end = lastDeclarationEnd declarations range
                                                    }

                                                generatedCode =
                                                    ElmCodeGen.componentModule
                                                        context.config.componentsNamespace
                                                        componentName
                                                        schema
                                            in
                                            [ Rule.errorWithFix
                                                { message = componentName ++ " component is missing propsDecoder"
                                                , details =
                                                    [ "This module should contain a propsDecoder and component definition matching the catalog."
                                                    , "Accept the fix to generate the correct types, decoder, and a view placeholder."
                                                    ]
                                                }
                                                range
                                                [ Fix.replaceRangeBy fullRange generatedCode ]
                                            ]

                                        _ ->
                                            []
                        in
                        ( errors, context )

                    Nothing ->
                        ( [], context )


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

        _ ->
            acc


finalEvaluation : Config -> ProjectContext -> List (Rule.Error { useErrorForModule : () })
finalEvaluation config projectCtx =
    case projectCtx.catalog of
        Nothing ->
            []

        Just catalog ->
            let
                missingModules =
                    Dict.keys catalog.components
                        |> List.filter (\name -> not (Set.member name projectCtx.seenModules))
                        |> List.sort

                missingModuleError =
                    if List.isEmpty missingModules then
                        []

                    else
                        let
                            fileList =
                                missingModules
                                    |> List.map
                                        (\name ->
                                            "src/"
                                                ++ String.replace "." "/" config.componentsNamespace
                                                ++ "/"
                                                ++ name
                                                ++ ".elm"
                                        )
                                    |> String.join ", "

                            nsPath =
                                String.replace "." "/" config.componentsNamespace

                            stubHint =
                                missingModules
                                    |> List.map
                                        (\name ->
                                            "  echo 'module "
                                                ++ config.componentsNamespace
                                                ++ "."
                                                ++ name
                                                ++ " exposing (..)' > src/"
                                                ++ nsPath
                                                ++ "/"
                                                ++ name
                                                ++ ".elm"
                                        )
                                    |> String.join " && \\\n"
                        in
                        [ Rule.globalError
                            { message = "Missing component modules: " ++ String.join ", " missingModules
                            , details =
                                [ "Create these files: " ++ fileList
                                , "Quick stub command:\n\nmkdir -p src/" ++ nsPath ++ " && \\\n" ++ stubHint
                                , "Then run elm-review --fix to fill them in."
                                ]
                            }
                        ]

                missingRegistryError =
                    if not projectCtx.registrySeen then
                        [ Rule.globalError
                            { message = "Missing registry module: " ++ config.componentsNamespace ++ ".Registry"
                            , details =
                                [ "Create: src/" ++ String.replace "." "/" config.componentsNamespace ++ "/Registry.elm"
                                , "Quick stub: echo 'module " ++ config.componentsNamespace ++ ".Registry exposing (..)' > src/" ++ String.replace "." "/" config.componentsNamespace ++ "/Registry.elm"
                                , "Then run elm-review --fix to fill it in."
                                ]
                            }
                        ]

                    else
                        []

                missingActionsError =
                    if not projectCtx.actionsSeen && not (Dict.isEmpty catalog.actions) then
                        [ Rule.globalError
                            { message = "Missing actions module: " ++ config.componentsNamespace ++ ".Actions"
                            , details =
                                [ "The catalog defines actions but no " ++ config.componentsNamespace ++ ".Actions module exists."
                                , "Run elm-review --fix to generate it."
                                ]
                            }
                        ]

                    else
                        []
            in
            missingModuleError ++ missingRegistryError ++ missingActionsError
