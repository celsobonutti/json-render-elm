module Catalog.Registry exposing (registry)

import Catalog.Components.Badge
import Catalog.Components.Button
import Catalog.Components.Card
import Catalog.Components.Image
import Catalog.Components.Input
import Catalog.Components.Stack
import Catalog.Components.Text
import Catalog.Components.Toggle
import Catalog.Functions
import Dict
import JsonRender.Actions as Actions
import JsonRender.Render exposing (Registry)


registry : Registry (Actions.Msg action)
registry =
    { components =
        Dict.fromList
            [ ( "Badge", Catalog.Components.Badge.component )
            , ( "Button", Catalog.Components.Button.component )
            , ( "Card", Catalog.Components.Card.component )
            , ( "Image", Catalog.Components.Image.component )
            , ( "Input", Catalog.Components.Input.component )
            , ( "Stack", Catalog.Components.Stack.component )
            , ( "Text", Catalog.Components.Text.component )
            , ( "Toggle", Catalog.Components.Toggle.component )
            ]
    , functions = Catalog.Functions.toFunctionDict Catalog.Functions.functions
    }
