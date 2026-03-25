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
                                , "Run elm-review --fix to generate it."
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
                                , "Run elm-review --fix to generate it."
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
                , """module Components.Actions exposing (Action(..))
type Action = Press
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


registry : Registry
registry =
    Dict.fromList
        [ ( "Button", Components.Button.component )
        , ( "Card", Components.Card.component )
        ]
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
                                , "Run elm-review --fix to generate it."
                                ]
                          }
                        ]
        , test "no errors when component, registry, and actions modules exist" <|
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
                , """module Components.Actions exposing (Action(..))
type Action = Press
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectNoErrors
        ]
