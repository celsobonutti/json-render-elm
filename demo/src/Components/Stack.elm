module Components.Stack exposing (StackProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class, style)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias StackProps =
    { direction : Maybe String
    , gap : Maybe Int
    }


propsDecoder : Dict String ResolvedValue -> Result String StackProps
propsDecoder =
    ResolvedValue.succeed StackProps
        |> ResolvedValue.optional "direction" ResolvedValue.string Nothing
        |> ResolvedValue.optional "gap" ResolvedValue.int Nothing


component : Component
component =
    register propsDecoder view


view : ComponentContext StackProps -> Html Msg
view ctx =
    let
        dirClass =
            case ctx.props.direction of
                Just "horizontal" ->
                    "jr-stack-horizontal"

                _ ->
                    "jr-stack-vertical"

        gapStyle =
            case ctx.props.gap of
                Just g ->
                    [ style "gap" (String.fromInt g ++ "px") ]

                Nothing ->
                    []
    in
    div ([ class ("jr-stack " ++ dirClass) ] ++ gapStyle) ctx.children
