port module Catalog.MiniApps.Button exposing (main)

import Browser
import Catalog.Components.Button as Button
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Resolve as Resolve


port propsIn : (Value -> msg) -> Sub msg


port actionsOut : Value -> Cmd msg


type alias Model =
    { props : Result String Button.ButtonProps
    }


type Msg
    = PropsUpdated Value
    | EmitAction String


decodeProps : Value -> Result String Button.ButtonProps
decodeProps flags =
    case Decode.decodeValue (Decode.field "props" Decode.value) flags of
        Ok propsValue ->
            case Decode.decodeValue (Decode.dict Resolve.valueDecoder) propsValue of
                Ok resolvedDict ->
                    Button.propsDecoder resolvedDict

                Err err ->
                    Err (Decode.errorToString err)

        Err err ->
            Err (Decode.errorToString err)


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { props = decodeProps flags }, actionsOut (Encode.object []) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PropsUpdated flags ->
            ( { model | props = decodeProps flags }, Cmd.none )

        EmitAction eventName ->
            ( model
            , actionsOut
                (Encode.object
                    [ ( "type", Encode.string "emit" )
                    , ( "event", Encode.string eventName )
                    ]
                )
            )


view : Model -> Html Msg
view model =
    case model.props of
        Ok props ->
            Button.viewStateless props EmitAction

        Err err ->
            Html.div [] [ Html.text ("Props error: " ++ err) ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    propsIn PropsUpdated


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
