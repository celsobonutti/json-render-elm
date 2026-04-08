port module Catalog.MiniApps.Stack exposing (main)

import Browser
import Catalog.Components.Stack as Stack
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Resolve as Resolve


port propsIn : (Value -> msg) -> Sub msg


port actionsOut : Value -> Cmd msg


type alias Model =
    { props : Result String Stack.StackProps
    }


type Msg
    = PropsUpdated Value


decodeProps : Value -> Result String Stack.StackProps
decodeProps flags =
    case Decode.decodeValue (Decode.field "props" Decode.value) flags of
        Ok propsValue ->
            case Decode.decodeValue (Decode.dict Resolve.valueDecoder) propsValue of
                Ok resolvedDict ->
                    Stack.propsDecoder resolvedDict

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


view : Model -> Html Msg
view model =
    case model.props of
        Ok props ->
            Stack.viewStateless props

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
