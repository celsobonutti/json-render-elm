module Catalog.Components.Button exposing (ButtonProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import JsonRender.Events as Events
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias ButtonProps =
    { label : String
    , variant : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String ButtonProps
propsDecoder =
    ResolvedValue.succeed ButtonProps
        |> ResolvedValue.required "label" ResolvedValue.string
        |> ResolvedValue.optional "variant" ResolvedValue.string Nothing


component : Component msg
component =
    register propsDecoder (\_ -> ()) view


view : ComponentContext ButtonProps () msg -> Html msg
view ctx =
    let
        variantClass =
            case ctx.props.variant of
                Just "secondary" ->
                    "jr-button-secondary"

                Just "danger" ->
                    "jr-button-danger"

                _ ->
                    "jr-button-primary"
    in
    button
        [ class ("jr-button " ++ variantClass)
        , Events.onClick (ctx.emit "press")
        ]
        [ text ctx.props.label ]
