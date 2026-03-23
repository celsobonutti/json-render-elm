module Components.Image exposing (component)

import Dict exposing (Dict)
import Html exposing (Html, img)
import Html.Attributes exposing (alt, class, src)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as Resolve exposing (ResolvedValue)


type alias ImageProps =
    { src : String
    , alt : String
    }


propsDecoder : Dict String ResolvedValue -> Result String ImageProps
propsDecoder =
    Resolve.succeed ImageProps
        |> Resolve.required "src" Resolve.string
        |> Resolve.required "alt" Resolve.string


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
