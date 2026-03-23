module Components.Button exposing (component)

import Dict exposing (Dict)
import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as Resolve exposing (ResolvedValue)


type alias ButtonProps =
    { label : String
    , variant : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String ButtonProps
propsDecoder =
    Resolve.succeed ButtonProps
        |> Resolve.required "label" Resolve.string
        |> Resolve.optional "variant" Resolve.string Nothing


component : Component
component =
    register propsDecoder view


view : ComponentContext ButtonProps -> Html Msg
view ctx =
    let
        variantClass =
            case ctx.props.variant of
                Just v ->
                    "jr-button-" ++ v

                Nothing ->
                    "jr-button-primary"
    in
    button
        [ class ("jr-button " ++ variantClass)
        , onClick (ctx.emit "press")
        ]
        [ text ctx.props.label ]
