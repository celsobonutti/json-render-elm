module CatalogSyncTest exposing (..)

import JsonRender.CatalogSync as CatalogSync
import Review.Test
import Test exposing (..)


sampleSchemaJson : String
sampleSchemaJson =
    """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","hasChildren":true}}}"""


baseConfig : CatalogSync.Config
baseConfig =
    { schemaJson = sampleSchemaJson
    , componentsNamespace = "Components"
    }


suite : Test
suite =
    describe "CatalogSync rule"
        [ test "reports missing component module and missing registry" <|
            \_ ->
                """module Main exposing (..)
import Html
main = Html.text "hello"
"""
                    |> Review.Test.run (CatalogSync.rule baseConfig)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Missing component module: Components.Card"
                          , details =
                                [ "The catalog defines a \"Card\" component but no Components.Card module exists."
                                , "Run elm-review --fix to generate it with the correct props type and decoder."
                                ]
                          }
                        , { message = "Missing registry module: Components.Registry"
                          , details =
                                [ "No Components.Registry module found."
                                , "Run elm-review --fix to regenerate it."
                                ]
                          }
                        ]
        , test "reports missing registry module" <|
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
                                [ "No Components.Registry module found."
                                , "Run elm-review --fix to regenerate it."
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
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule baseConfig)
                    |> Review.Test.expectNoErrors
        , test "reports incomplete registry" <|
            \_ ->
                let
                    twoComponentSchema =
                        """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","hasChildren":true},"Button":{"props":{"type":"object","properties":{"label":{"type":"string"}},"required":["label"]},"description":"A button","hasChildren":false}}}"""

                    twoConfig =
                        { schemaJson = twoComponentSchema
                        , componentsNamespace = "Components"
                        }
                in
                [ """module Components.Card exposing (..)
type alias CardProps = { title : String }
component = ()
"""
                , """module Components.Button exposing (..)
type alias ButtonProps = { label : String }
component = ()
"""
                , """module Components.Registry exposing (registry)
import Dict
import Components.Card
registry = Dict.fromList [ ( "Card", Components.Card.component ) ]
"""
                ]
                    |> Review.Test.runOnModules (CatalogSync.rule twoConfig)
                    |> Review.Test.expectGlobalErrors
                        [ { message = "Registry is missing components: Button"
                          , details =
                                [ "The registry module does not include all catalog components."
                                , "Run elm-review --fix to regenerate it."
                                ]
                          }
                        ]
        ]
