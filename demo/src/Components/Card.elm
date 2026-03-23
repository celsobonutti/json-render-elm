module Components.Card exposing (component)

import Dict exposing (Dict)
import Html exposing (Html, div, h3, p, text)
import Html.Attributes exposing (class)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as Resolve exposing (ResolvedValue)


type alias CardProps =
    { title : String
    , subtitle : Maybe String
    }


propsDecoder : Dict String ResolvedValue -> Result String CardProps
propsDecoder =
    Resolve.succeed CardProps
        |> Resolve.required "title" Resolve.string
        |> Resolve.optional "subtitle" Resolve.string Nothing


component : Component
component =
    register propsDecoder view


view : ComponentContext CardProps -> Html Msg
view ctx =
    div [ class "jr-card" ]
        ([ h3 [ class "jr-card-title" ] [ text ctx.props.title ]
         ]
            ++ (case ctx.props.subtitle of
                    Just sub ->
                        [ p [ class "jr-card-subtitle" ] [ text sub ] ]

                    Nothing ->
                        []
               )
            ++ [ div [ class "jr-card-body" ] ctx.children ]
        )
