module Components.Badge exposing (BadgeProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias BadgeProps =
    { color : Maybe String
    , label : String
    }


propsDecoder : Dict String ResolvedValue -> Result String BadgeProps
propsDecoder =
    ResolvedValue.succeed BadgeProps
        |> ResolvedValue.optional "color" ResolvedValue.string Nothing
        |> ResolvedValue.required "label" ResolvedValue.string


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
