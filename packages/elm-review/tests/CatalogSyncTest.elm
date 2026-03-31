module CatalogSyncTest exposing (..)

import JsonRender.CatalogSync as CatalogSync
import Review.Test
import Test exposing (..)


sampleSchemaJson : String
sampleSchemaJson =
    """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","slots":["default"]}},"actions":{"press":{"params":{"type":"object","properties":{},"required":[]},"description":"Generic button press"}}}"""


baseConfig : CatalogSync.Config
baseConfig =
    { schemaJson = sampleSchemaJson
    , componentsNamespace = "Components"
    }


suite : Test
suite =
    describe "CatalogSync rule"
        [ test "reports missing component module, registry, and actions" <|
            \_ ->
                """module Main exposing (..)
import Html
main = Html.text "hello"
"""
                    |> Review.Test.run (CatalogSync.rule baseConfig)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Missing component modules: Card"
                          , details =
                                [ "Create these files: src/Components/Card.elm"
                                , "Quick stub command:\n\nmkdir -p src/Components && \\\n  echo 'module Components.Card exposing (..)' > src/Components/Card.elm"
                                , "Then run elm-review --fix to fill them in."
                                ]
                          }
                        , { message = "Missing registry module: Components.Registry"
                          , details =
                                [ "Create: src/Components/Registry.elm"
                                , "Quick stub: echo 'module Components.Registry exposing (..)' > src/Components/Registry.elm"
                                , "Then run elm-review --fix to fill it in."
                                ]
                          }
                        , { message = "Missing actions module: Components.Actions"
                          , details =
                                [ "The catalog defines actions but no Components.Actions module exists."
                                , "Quick stub:\n\nmkdir -p src/Components && echo 'module Components.Actions exposing (..)' > src/Components/Actions.elm"
                                , "Then run elm-review --fix to fill it in."
                                ]
                          }
                        ]
        , test "reports missing registry and actions modules" <|
            \_ ->
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Missing registry module: Components.Registry"
                          , details =
                                [ "Create: src/Components/Registry.elm"
                                , "Quick stub: echo 'module Components.Registry exposing (..)' > src/Components/Registry.elm"
                                , "Then run elm-review --fix to fill it in."
                                ]
                          }
                        , { message = "Missing actions module: Components.Actions"
                          , details =
                                [ "The catalog defines actions but no Components.Actions module exists."
                                , "Quick stub:\n\nmkdir -p src/Components && echo 'module Components.Actions exposing (..)' > src/Components/Actions.elm"
                                , "Then run elm-review --fix to fill it in."
                                ]
                          }
                        ]
        , test "no errors when component and registry exist" <|
            \_ ->
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                , """module Components.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)
type Action = Press
actionConfig functions = ()
decodeAction name params = Ok Press
handleAction action model = ()
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectNoErrors
        , test "reports incomplete registry" <|
            \_ ->
                let
                    twoComponentSchema =
                        """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","slots":["default"]},"Button":{"props":{"type":"object","properties":{"label":{"type":"string"}},"required":["label"]},"description":"A button","slots":[]}}}"""

                    twoConfig =
                        { schemaJson = twoComponentSchema
                        , componentsNamespace = "Components"
                        }
                in
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
"""
                , """module Components.Button exposing (..)
type alias ButtonProps = { label : String }
propsDecoder = identity
component = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule twoConfig)
                    |> Review.Test.expectErrorsForModules
                        [ ( "Components.Registry"
                          , [ Review.Test.error
                                { message = "Registry is missing components: Button"
                                , details =
                                    [ "The registry module does not include all catalog components."
                                    , "Accept the fix to regenerate it."
                                    ]
                                , under = "module Components.Registry exposing (registry)"
                                }
                                |> Review.Test.whenFixed """module Components.Registry exposing (registry)

import Dict
import JsonRender.Render exposing (Registry)
import Components.Button
import Components.Card


registry : Registry msg
registry =
    { components =
        Dict.fromList
        [ ( "Button", Components.Button.component )
        , ( "Card", Components.Card.component )
        ]
    , functions = Dict.empty
    }
"""
                            ]
                          )
                        ]
        , test "reports missing actions module" <|
            \_ ->
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Missing actions module: Components.Actions"
                          , details =
                                [ "The catalog defines actions but no Components.Actions module exists."
                                , "Quick stub:\n\nmkdir -p src/Components && echo 'module Components.Actions exposing (..)' > src/Components/Actions.elm"
                                , "Then run elm-review --fix to fill it in."
                                ]
                          }
                        ]
        , test "no errors when component, registry, and actions modules exist with correct type" <|
            \_ ->
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                , """module Components.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)
type Action = Press
actionConfig functions = ()
decodeAction name params = Ok Press
handleAction action model = ()
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectNoErrors
        , test "reports outdated actions module when variants are wrong" <|
            \_ ->
                let
                    schemaWithTwoActions =
                        """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","slots":["default"]}},"actions":{"press":{"params":{"type":"object","properties":{},"required":[]},"description":"Generic button press"},"export":{"params":{"type":"object","properties":{"format":{"type":"string"}},"required":["format"]},"description":"Export data"}}}"""

                    twoActionsConfig =
                        { schemaJson = schemaWithTwoActions
                        , componentsNamespace = "Components"
                        }
                in
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                , """module Components.Actions exposing (Action(..))
type Action = Press
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule twoActionsConfig)
                    |> Review.Test.expectErrorsForModules
                        [ ( "Components.Actions"
                          , [ Review.Test.error
                                { message = "Actions module is missing variants: Export, actionConfig, handleAction"
                                , details =
                                    [ "The Actions module does not match the catalog actions."
                                    , "Accept the fix to regenerate it."
                                    ]
                                , under = "module Components.Actions exposing (Action(..))"
                                }
                                |> Review.Test.whenFixed ("module Components.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)\n\nimport Dict exposing (Dict)\nimport Json.Decode as Decode\nimport Json.Encode exposing (Value)\nimport JsonRender.Actions as Actions\n\n\ntype alias ExportParams =\n    { format : String\n    }\n\n\ntype Action\n    = Export ExportParams\n    | Press\n\n\ndecodeAction : String -> Dict String Value -> Result String Action\ndecodeAction name params =\n    case name of\n        \"export\" ->\n            case Dict.get \"format\" params of\n                Just format_raw ->\n                    case Decode.decodeValue Decode.string format_raw of\n                        Ok format ->\n                            Ok (Export { format = format })\n\n                        Err _ ->\n                            Err \"format must be a String\"\n\n                Nothing ->\n                    Err \"missing required param format\"\n\n        \"press\" ->\n            Ok Press\n\n        _ ->\n            Err (\"Unknown action: \" ++ name)\n\n\nhandleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )\nhandleAction action model =\n    ()\n\n\nactionConfig : Actions.ActionConfig Action\nactionConfig =\n    { handleAction = handleAction\n    , decodeAction = decodeAction\n    , functions = Dict.empty\n    }\n")
                            ]
                          )
                        ]
        , test "no errors when actions module has all variants including decodeAction" <|
            \_ ->
                let
                    schemaWithTwoActions =
                        """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","slots":["default"]}},"actions":{"press":{"params":{"type":"object","properties":{},"required":[]},"description":"Generic button press"},"export":{"params":{"type":"object","properties":{"format":{"type":"string"}},"required":["format"]},"description":"Export data"}}}"""

                    twoActionsConfig =
                        { schemaJson = schemaWithTwoActions
                        , componentsNamespace = "Components"
                        }
                in
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                , """module Components.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)
type Action = Export ExportParams | Press
actionConfig functions = ()
decodeAction name params = Ok Press
handleAction action model = ()
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule twoActionsConfig)
                    |> Review.Test.expectNoErrors
        , test "reports missing actionConfig and handleAction when variants are correct" <|
            \_ ->
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                , """module Components.Actions exposing (Action(..), decodeAction)
type Action = Press
decodeAction name params = Ok Press
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectErrorsForModules
                        [ ( "Components.Actions"
                          , [ Review.Test.error
                                { message = "Actions module is missing actionConfig, handleAction"
                                , details =
                                    [ "The Actions module does not match the catalog actions."
                                    , "Accept the fix to regenerate it."
                                    ]
                                , under = "module Components.Actions exposing (Action(..), decodeAction)"
                                }
                                |> Review.Test.whenFixed ("module Components.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)\n\nimport Dict exposing (Dict)\nimport Json.Decode as Decode\nimport Json.Encode exposing (Value)\nimport JsonRender.Actions as Actions\n\n\ntype Action\n    = Press\n\n\ndecodeAction : String -> Dict String Value -> Result String Action\ndecodeAction name params =\n    case name of\n        \"press\" ->\n            Ok Press\n\n        _ ->\n            Err (\"Unknown action: \" ++ name)\n\n\nhandleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )\nhandleAction action model =\n    ()\n\n\nactionConfig : Actions.ActionConfig Action\nactionConfig =\n    { handleAction = handleAction\n    , decodeAction = decodeAction\n    , functions = Dict.empty\n    }\n")
                            ]
                          )
                        ]
        , test "no actions in catalog means no actions module needed" <|
            \_ ->
                let
                    noActionsSchema =
                        """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","slots":["default"]}}}"""

                    noActionsConfig =
                        { schemaJson = noActionsSchema
                        , componentsNamespace = "Components"
                        }
                in
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule noActionsConfig)
                    |> Review.Test.expectNoErrors
        , test "fixes stub actions module" <|
            \_ ->
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                , """module Components.Actions exposing (..)
placeholder = ()
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectErrorsForModules
                        [ ( "Components.Actions"
                          , [ Review.Test.error
                                { message = "Actions module is missing variants: Press, actionConfig, handleAction"
                                , details =
                                    [ "The Actions module does not match the catalog actions."
                                    , "Accept the fix to regenerate it."
                                    ]
                                , under = "module Components.Actions exposing (..)"
                                }
                                |> Review.Test.whenFixed ("module Components.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)\n\nimport Dict exposing (Dict)\nimport Json.Decode as Decode\nimport Json.Encode exposing (Value)\nimport JsonRender.Actions as Actions\n\n\ntype Action\n    = Press\n\n\ndecodeAction : String -> Dict String Value -> Result String Action\ndecodeAction name params =\n    case name of\n        \"press\" ->\n            Ok Press\n\n        _ ->\n            Err (\"Unknown action: \" ++ name)\n\n\nhandleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )\nhandleAction action model =\n    ()\n\n\nactionConfig : Actions.ActionConfig Action\nactionConfig =\n    { handleAction = handleAction\n    , decodeAction = decodeAction\n    , functions = Dict.empty\n    }\n")
                            ]
                          )
                        ]
        , test "actionConfig uses Functions module when catalog has functions" <|
            \_ ->
                let
                    schemaWithFunctions =
                        """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","slots":["default"]}},"actions":{"press":{"params":{"type":"object","properties":{},"required":[]},"description":"Press"}},"functions":{"shout":{"params":{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]},"returnType":{"type":"string"},"description":"Uppercase"}}}"""

                    fnConfig =
                        { schemaJson = schemaWithFunctions
                        , componentsNamespace = "Components"
                        }
                in
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
propsDecoder = identity
component = ()
view ctx = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
import Components.Functions
registry = { components = Dict.fromList [ ( "Card", Components.Card.component ) ], functions = Dict.empty }
"""
                , """module Components.Functions exposing (Functions, functions, toFunctionDict)
type alias ShoutParams = { text : String }
type alias Functions = { shout : ShoutParams -> String }
functions = { shout = identity }
toFunctionDict fns = Dict.empty
"""
                , """module Components.Actions exposing (..)
placeholder = ()
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule fnConfig)
                    |> Review.Test.expectErrorsForModules
                        [ ( "Components.Actions"
                          , [ Review.Test.error
                                { message = "Actions module is missing variants: Press, actionConfig, handleAction"
                                , details =
                                    [ "The Actions module does not match the catalog actions."
                                    , "Accept the fix to regenerate it."
                                    ]
                                , under = "module Components.Actions exposing (..)"
                                }
                                |> Review.Test.whenFixed ("module Components.Actions exposing (Action(..), actionConfig, decodeAction, handleAction)\n\nimport Dict exposing (Dict)\nimport Json.Decode as Decode\nimport Json.Encode exposing (Value)\nimport JsonRender.Actions as Actions\nimport Components.Functions\n\n\ntype Action\n    = Press\n\n\ndecodeAction : String -> Dict String Value -> Result String Action\ndecodeAction name params =\n    case name of\n        \"press\" ->\n            Ok Press\n\n        _ ->\n            Err (\"Unknown action: \" ++ name)\n\n\nhandleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )\nhandleAction action model =\n    ()\n\n\nactionConfig : Actions.ActionConfig Action\nactionConfig =\n    { handleAction = handleAction\n    , decodeAction = decodeAction\n    , functions = Components.Functions.toFunctionDict Components.Functions.functions\n    }\n")
                            ]
                          )
                        ]
        ]
