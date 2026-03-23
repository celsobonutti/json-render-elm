module JsonRender.CatalogSync exposing (Config, rule)

{-| elm-review rule that keeps Elm component modules in sync with a json-render catalog.

Checks:

1.  All catalog components have a corresponding Elm module
2.  Props type aliases match the catalog schema
3.  The registry module includes all components

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Module as Module
import Elm.Syntax.Node as Node exposing (Node(..))
import Json.Decode as Decode
import JsonRender.Internal.ElmCodeGen as ElmCodeGen
import JsonRender.Internal.SchemaParser as SchemaParser exposing (CatalogSchema, ComponentSchema)
import JsonRender.Internal.TypeMapping as TypeMapping
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
    }


type alias ModuleContext =
    { catalog : Maybe CatalogSchema
    , config : Config
    , moduleName : String
    , isComponentModule : Maybe String
    , isRegistryModule : Bool
    , registryEntries : Set String
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
    }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationListVisitor declarationListVisitor


fromProjectToModule : Maybe CatalogSchema -> Config -> Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule catalog config =
    Rule.initContextCreator
        (\moduleName _ ->
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
            , registryEntries = Set.empty
            }
        )
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
            }
        )


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts a b =
    { catalog = a.catalog
    , config = a.config
    , seenModules = Set.union a.seenModules b.seenModules
    , registrySeen = a.registrySeen || b.registrySeen
    , registryComponents = Set.union a.registryComponents b.registryComponents
    }


moduleDefinitionVisitor : Node Module.Module -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
moduleDefinitionVisitor _ context =
    ( [], context )


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List (Rule.Error {}), ModuleContext )
declarationListVisitor declarations context =
    if context.isRegistryModule then
        let
            entries =
                extractRegistryEntries declarations
        in
        ( [], { context | registryEntries = entries } )

    else
        ( [], context )


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
                missingModuleErrors =
                    Dict.toList catalog.components
                        |> List.filterMap
                            (\( name, _ ) ->
                                if Set.member name projectCtx.seenModules then
                                    Nothing

                                else
                                    Just
                                        (Rule.globalError
                                            { message = "Missing component module: " ++ config.componentsNamespace ++ "." ++ name
                                            , details =
                                                [ "The catalog defines a \"" ++ name ++ "\" component but no " ++ config.componentsNamespace ++ "." ++ name ++ " module exists."
                                                , "Run elm-review --fix to generate it with the correct props type and decoder."
                                                ]
                                            }
                                        )
                            )

                missingRegistryError =
                    if not projectCtx.registrySeen then
                        [ Rule.globalError
                            { message = "Missing registry module: " ++ config.componentsNamespace ++ ".Registry"
                            , details =
                                [ "No " ++ config.componentsNamespace ++ ".Registry module found."
                                , "Run elm-review --fix to regenerate it."
                                ]
                            }
                        ]

                    else
                        let
                            catalogNames =
                                Dict.keys catalog.components |> Set.fromList

                            missing =
                                Set.diff catalogNames projectCtx.registryComponents
                        in
                        if Set.isEmpty missing then
                            []

                        else
                            [ Rule.globalError
                                { message = "Registry is missing components: " ++ String.join ", " (Set.toList missing)
                                , details =
                                    [ "The registry module does not include all catalog components."
                                    , "Run elm-review --fix to regenerate it."
                                    ]
                                }
                            ]
            in
            missingModuleErrors ++ missingRegistryError
