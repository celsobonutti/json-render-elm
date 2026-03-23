module Components.Input exposing (InputProps, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, placeholder, type_)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias InputProps =
    { label : Maybe String
    , placeholder : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String InputProps
propsDecoder =
    ResolvedValue.succeed InputProps
        |> ResolvedValue.optional "label" ResolvedValue.string Nothing
        |> ResolvedValue.optional "placeholder" ResolvedValue.string Nothing


component : Component
component =
    register propsDecoder view


view : ComponentContext InputProps -> Html Msg
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
                    ]
                    []
               ]
        )
