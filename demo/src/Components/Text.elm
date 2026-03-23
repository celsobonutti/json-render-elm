module Components.Text exposing (TextProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias TextProps =
    { content : String
    , size : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String TextProps
propsDecoder =
    ResolvedValue.succeed TextProps
        |> ResolvedValue.required "content" ResolvedValue.string
        |> ResolvedValue.optional "size" ResolvedValue.string Nothing


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
