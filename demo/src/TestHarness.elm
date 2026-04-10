port module TestHarness exposing (main)

import Browser
import Catalog.Actions exposing (Action(..), decodeAction)
import Catalog.Registry exposing (registry)
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Json.Encode as Encode exposing (Value)
import JsonRender
import JsonRender.Actions as Actions
import Random


port jsonRenderSpecIn : (Value -> msg) -> Sub msg


port jsonRenderStateIn : (Value -> msg) -> Sub msg


port testActionOut : Value -> Cmd msg


port testDecodeErrorOut : String -> Cmd msg


port componentPortIn : (Value -> msg) -> Sub msg


port componentPortOut : Value -> Cmd msg


type alias Model =
    { jsonRender : JsonRender.Model Action
    }


type Msg
    = SpecReceived Value
    | StateReceived Value
    | JsonRenderMsg (Actions.Msg Action)


init : Int -> ( Model, Cmd Msg )
init randomSeed =
    ( { jsonRender = JsonRender.init (Random.initialSeed randomSeed) }
    , Cmd.none
    )


handleAction : Action -> Actions.Model Action -> ( Actions.Model Action, Cmd (Actions.Msg Action) )
handleAction action model =
    ( model, testActionOut (encodeAction action) )


encodeAction : Action -> Value
encodeAction action =
    case action of
        Press ->
            Encode.object [ ( "name", Encode.string "press" ) ]

        Export { format } ->
            Encode.object
                [ ( "name", Encode.string "export" )
                , ( "params", Encode.object [ ( "format", Encode.string format ) ] )
                ]


app : JsonRender.App Action Model Msg
app =
    JsonRender.create
        { actionConfig =
            { handleAction = handleAction
            , decodeAction = decodeAction
            }
        , registry = registry
        , toMsg = JsonRenderMsg
        , getModel = .jsonRender
        , setModel = \jr model -> { model | jsonRender = jr }
        , componentPortOut = Just componentPortOut
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpecReceived val ->
            case JsonRender.receiveSpec val model.jsonRender of
                Ok jr ->
                    ( { model | jsonRender = jr }, Cmd.none )

                Err err ->
                    ( model, testDecodeErrorOut err )

        StateReceived val ->
            let
                jr =
                    model.jsonRender
            in
            ( { model | jsonRender = { jr | state = val } }, Cmd.none )

        JsonRenderMsg actionMsg ->
            app.update actionMsg model


view : Model -> Html Msg
view model =
    div [ id "render-root" ]
        [ case model.jsonRender.spec of
            Just _ ->
                app.render model

            Nothing ->
                div [ id "no-spec" ] []
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsonRenderSpecIn SpecReceived
        , jsonRenderStateIn StateReceived
        , componentPortIn (JsonRender.decodePortIn JsonRenderMsg)
        ]


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
