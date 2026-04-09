module Catalog.Registry exposing (registry)

import Catalog.Components.Badge
import Catalog.Components.Button
import Catalog.Components.Card
import Catalog.Components.Dropdown
import Catalog.Components.Image
import Catalog.Components.Input
import Catalog.Components.Stack
import Catalog.Components.Text
import Catalog.Functions
import Dict
import JsonRender.Render exposing (Registry)


registry : Registry msg
registry =
    { components =
        Dict.fromList
            [ ( "Badge", Catalog.Components.Badge.component )
            , ( "Button", Catalog.Components.Button.component )
            , ( "Card", Catalog.Components.Card.component )
            , ( "Dropdown", Catalog.Components.Dropdown.component )
            , ( "Image", Catalog.Components.Image.component )
            , ( "Input", Catalog.Components.Input.component )
            , ( "Stack", Catalog.Components.Stack.component )
            , ( "Text", Catalog.Components.Text.component )
            ]
    , functions = Catalog.Functions.toFunctionDict Catalog.Functions.functions
    }
