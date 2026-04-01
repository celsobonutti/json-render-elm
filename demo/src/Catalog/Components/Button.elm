module Catalog.Components.Button exposing (ButtonProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Json.Encode exposing (Value)
import JsonRender.Bind as Bind
import JsonRender.Events as Events exposing (EventHandle)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type PrimaryOrSecondaryOrDanger
    = Primary
    | Secondary
    | Danger


primaryOrSecondaryOrDangerFromString : String -> Result String PrimaryOrSecondaryOrDanger
primaryOrSecondaryOrDangerFromString str =
    case str of
        "primary" ->
            Ok Primary

        "secondary" ->
            Ok Secondary

        "danger" ->
            Ok Danger

        _ ->
            Err ("Unknown value: " ++ str ++ ". Expected one of: primary, secondary, danger")


primaryOrSecondaryOrDangerToString : PrimaryOrSecondaryOrDanger -> String
primaryOrSecondaryOrDangerToString value =
    case value of
        Primary ->
            "primary"

        Secondary ->
            "secondary"

        Danger ->
            "danger"


type alias ButtonProps =
    { label : String, variant : Maybe PrimaryOrSecondaryOrDanger }


type alias ButtonBindings msg =
    { label : Maybe (String -> EventHandle msg), variant : Maybe (PrimaryOrSecondaryOrDanger -> EventHandle msg) }


propsDecoder : Dict String ResolvedValue -> Result String ButtonProps
propsDecoder =
    ResolvedValue.succeed ButtonProps
        |> ResolvedValue.required "label" ResolvedValue.string
        |> ResolvedValue.optional
            "variant"
            (\rv -> ResolvedValue.string rv |> Result.andThen primaryOrSecondaryOrDangerFromString)
            Nothing


bindingsDecoder : Dict String (Value -> EventHandle msg) -> ButtonBindings msg
bindingsDecoder =
    Bind.succeed ButtonBindings
        |> Bind.bindableTyped "label" Json.Encode.string
        |> Bind.bindableTyped "variant" (Json.Encode.string << primaryOrSecondaryOrDangerToString)


component : Component msg
component =
    register propsDecoder bindingsDecoder view


view : ComponentContext ButtonProps (ButtonBindings msg) msg -> Html msg
view ctx =
    let
        variantClass =
            case ctx.props.variant of
                Just Secondary ->
                    "jr-button-secondary"

                Just Danger ->
                    "jr-button-danger"

                _ ->
                    "jr-button-primary"
    in
    button
        [ class ("jr-button " ++ variantClass)
        , Events.onClick (ctx.emit "press")
        ]
        [ text ctx.props.label ]
