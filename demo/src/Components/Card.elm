module Components.Card exposing (CardProps, component, propsDecoder)

import Components.Actions exposing (Action)
import Dict exposing (Dict)
import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (class)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


type alias CardProps =
    { subtitle : Maybe String
    , title : String
    }


propsDecoder : Dict String ResolvedValue -> Result String CardProps
propsDecoder =
    ResolvedValue.succeed CardProps
        |> ResolvedValue.optional "subtitle" ResolvedValue.string Nothing
        |> ResolvedValue.required "title" ResolvedValue.string


component : Component Action
component =
    register propsDecoder (\_ -> ()) view


view : ComponentContext CardProps () Action -> Html (Msg Action)
view ctx =
    div [ class "jr-card" ]
        ([ h3 [ class "jr-card-title" ] [ text ctx.props.title ] ]
            ++ (case ctx.props.subtitle of
                    Just sub ->
                        [ p [ class "jr-card-subtitle" ] [ text sub ] ]

                    Nothing ->
                        []
               )
            ++ [ div [ class "jr-card-body" ] ctx.children ]
        )
