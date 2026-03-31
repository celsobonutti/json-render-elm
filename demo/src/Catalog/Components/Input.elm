module Catalog.Components.Input exposing (InputBindings, InputProps, component, inputBindingsDecoder, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, placeholder, type_)
import JsonRender.Events as Events exposing (EventHandle)
import Json.Encode as Encode exposing (Value)
import JsonRender.Bind as Bind
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias InputProps =
    { label : Maybe String
    , placeholder : Maybe String
    , value : Maybe String
    }


type alias InputBindings msg =
    { label : Maybe (Value -> EventHandle msg)
    , placeholder : Maybe (Value -> EventHandle msg)
    , value : Maybe (Value -> EventHandle msg)
    }


propsDecoder : Dict String ResolvedValue -> Result String InputProps
propsDecoder =
    ResolvedValue.succeed InputProps
        |> ResolvedValue.optional "label" ResolvedValue.string Nothing
        |> ResolvedValue.optional "placeholder" ResolvedValue.string Nothing
        |> ResolvedValue.optional "value" ResolvedValue.string Nothing


inputBindingsDecoder : Dict String (Value -> EventHandle msg) -> InputBindings msg
inputBindingsDecoder =
    Bind.succeed InputBindings
        |> Bind.bindable "label"
        |> Bind.bindable "placeholder"
        |> Bind.bindable "value"


component : Component msg
component =
    register propsDecoder inputBindingsDecoder view


view : ComponentContext InputProps (InputBindings msg) msg -> Html msg
view ctx =
    div [ class "jr-input-wrapper" ]
        ((case ctx.props.label of
            Just lbl ->
                [ label [ class "jr-input-label" ] [ text lbl ] ]

            Nothing ->
                []
         )
            ++ [ input
                    [ type_ "text"
                    , class "jr-input"
                    , placeholder (Maybe.withDefault "" ctx.props.placeholder)
                    , Html.Attributes.value (Maybe.withDefault "" ctx.props.value)
                    , case ctx.bindings.value of
                        Just setValue ->
                            Events.onInput (\s -> setValue (Encode.string s))

                        Nothing ->
                            class ""
                    ]
                    []
               ]
        )
