module Catalog.Components.Image exposing (ImageProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, img)
import Html.Attributes exposing (alt, class, src)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias ImageProps =
    { alt : String
    , src : String
    }


propsDecoder : Dict String ResolvedValue -> Result String ImageProps
propsDecoder =
    ResolvedValue.succeed ImageProps
        |> ResolvedValue.required "alt" ResolvedValue.string
        |> ResolvedValue.required "src" ResolvedValue.string


component : Component msg
component =
    register propsDecoder (\_ -> ()) view


view : ComponentContext ImageProps () msg -> Html msg
view ctx =
    img
        [ src ctx.props.src
        , alt ctx.props.alt
        , class "jr-image"
        ]
        []
