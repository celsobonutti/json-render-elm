module Components.Input exposing (component)

import Dict exposing (Dict)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (class, placeholder, type_)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as Resolve exposing (ResolvedValue)


type alias InputProps =
    { placeholder : Maybe String
    , label : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String InputProps
propsDecoder =
    Resolve.succeed InputProps
        |> Resolve.optional "placeholder" Resolve.string Nothing
        |> Resolve.optional "label" Resolve.string Nothing


component : Component
component =
    register propsDecoder view


view : ComponentContext InputProps -> Html Msg
view ctx =
    div [ class "jr-input-wrapper" ]
        (case ctx.props.label of
            Just lbl ->
                [ label [ class "jr-input-label" ] [ text lbl ]
                , input
                    [ type_ "text"
                    , class "jr-input"
                    , placeholder (Maybe.withDefault "" ctx.props.placeholder)
                    ]
                    []
                ]

            Nothing ->
                [ input
                    [ type_ "text"
                    , class "jr-input"
                    , placeholder (Maybe.withDefault "" ctx.props.placeholder)
                    ]
                    []
                ]
        )
