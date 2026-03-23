module Components.Badge exposing (component)

import Dict exposing (Dict)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as Resolve exposing (ResolvedValue)


type alias BadgeProps =
    { label : String
    , color : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String BadgeProps
propsDecoder =
    Resolve.succeed BadgeProps
        |> Resolve.required "label" Resolve.string
        |> Resolve.optional "color" Resolve.string Nothing


component : Component
component =
    register propsDecoder view


view : ComponentContext BadgeProps -> Html Msg
view ctx =
    let
        colorClass =
            case ctx.props.color of
                Just c ->
                    "jr-badge-" ++ c

                Nothing ->
                    "jr-badge-gray"
    in
    span [ class ("jr-badge " ++ colorClass) ] [ text ctx.props.label ]
