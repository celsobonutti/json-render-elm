module Components.Registry exposing (registry)

import Components.Badge
import Components.Button
import Components.Card
import Components.Image
import Components.Input
import Components.Stack
import Components.Text
import Dict
import JsonRender.Render exposing (Registry)


registry : Registry
registry =
    Dict.fromList
        [ ( "Card", Components.Card.component )
        , ( "Button", Components.Button.component )
        , ( "Text", Components.Text.component )
        , ( "Input", Components.Input.component )
        , ( "Stack", Components.Stack.component )
        , ( "Image", Components.Image.component )
        , ( "Badge", Components.Badge.component )
        ]
