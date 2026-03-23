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
        [ ( "Badge", Components.Badge.component )
        , ( "Button", Components.Button.component )
        , ( "Card", Components.Card.component )
        , ( "Image", Components.Image.component )
        , ( "Input", Components.Input.component )
        , ( "Stack", Components.Stack.component )
        , ( "Text", Components.Text.component )
        ]
