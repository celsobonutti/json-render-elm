module Components.Image exposing (ImageProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, img)
import Html.Attributes exposing (alt, class, src)
import JsonRender.Actions exposing (Msg)
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


component : Component
component =
    register propsDecoder view


view : ComponentContext ImageProps -> Html Msg
view ctx =
    img
        [ src ctx.props.src
        , alt ctx.props.alt
        , class "jr-image"
        ]
        []
