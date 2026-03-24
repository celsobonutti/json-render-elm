port module TestHarness exposing (main)

import Browser
import Components.Registry exposing (registry)
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Actions as Actions
import JsonRender.Render as Render
import JsonRender.Spec as Spec exposing (Spec)


port jsonRenderSpecIn : (Value -> msg) -> Sub msg


port jsonRenderActionOut : Value -> Cmd msg


port jsonRenderStateIn : (Value -> msg) -> Sub msg


type alias Model =
    { spec : Maybe Spec
    , renderState : Value
    }


type Msg
    = SpecReceived Value
    | StateReceived Value
    | JsonRenderMsg Actions.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { spec = Nothing
      , renderState = Encode.object []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpecReceived val ->
            case Decode.decodeValue Spec.decoder val of
                Ok spec ->
                    ( { model | spec = Just spec }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        StateReceived val ->
            ( { model | renderState = val }, Cmd.none )

        JsonRenderMsg actionMsg ->
            case actionMsg of
                Actions.CustomAction name params ->
                    ( model, jsonRenderActionOut (Actions.encodeAction name params) )

                _ ->
                    let
                        actionsModel =
                            { spec = model.spec, state = model.renderState }

                        ( newActionsModel, _ ) =
                            Actions.update actionMsg actionsModel
                    in
                    ( { model | renderState = newActionsModel.state }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ id "render-root" ]
        [ case model.spec of
            Just spec ->
                Html.map JsonRenderMsg
                    (Render.render registry model.renderState spec)

            Nothing ->
                div [ id "no-spec" ] []
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsonRenderSpecIn SpecReceived
        , jsonRenderStateIn StateReceived
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
