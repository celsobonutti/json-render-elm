module Components.Text exposing (component)

import Dict exposing (Dict)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as Resolve exposing (ResolvedValue)


type alias TextProps =
    { content : String
    , size : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String TextProps
propsDecoder =
    Resolve.succeed TextProps
        |> Resolve.required "content" Resolve.string
        |> Resolve.optional "size" Resolve.string Nothing


component : Component
component =
    register propsDecoder view


view : ComponentContext TextProps -> Html Msg
view ctx =
    let
        sizeClass =
            case ctx.props.size of
                Just s ->
                    "jr-text-" ++ s

                Nothing ->
                    "jr-text-md"
    in
    span [ class ("jr-text " ++ sizeClass) ] [ text ctx.props.content ]
