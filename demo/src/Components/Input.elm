module Components.Input exposing (InputBindings, InputProps, component, inputBindingsDecoder, propsDecoder)

import Components.Actions exposing (Action)
import Dict exposing (Dict)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events
import Json.Encode as Encode exposing (Value)
import JsonRender.Actions exposing (Msg)
import JsonRender.Bind as Bind
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias InputProps =
    { label : Maybe String
    , placeholder : Maybe String
    , value : Maybe String
    }


type alias InputBindings =
    { label : Maybe (Value -> Msg Action)
    , placeholder : Maybe (Value -> Msg Action)
    , value : Maybe (Value -> Msg Action)
    }


propsDecoder : Dict String ResolvedValue -> Result String InputProps
propsDecoder =
    ResolvedValue.succeed InputProps
        |> ResolvedValue.optional "label" ResolvedValue.string Nothing
        |> ResolvedValue.optional "placeholder" ResolvedValue.string Nothing
        |> ResolvedValue.optional "value" ResolvedValue.string Nothing


inputBindingsDecoder : Dict String (Value -> Msg Action) -> InputBindings
inputBindingsDecoder =
    Bind.succeed InputBindings
        |> Bind.bindable "label"
        |> Bind.bindable "placeholder"
        |> Bind.bindable "value"


component : Component Action
component =
    register propsDecoder inputBindingsDecoder view


view : ComponentContext InputProps InputBindings Action -> Html (Msg Action)
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
                            Html.Events.onInput (\s -> setValue (Encode.string s))

                        Nothing ->
                            class ""
                    ]
                    []
               ]
        )
