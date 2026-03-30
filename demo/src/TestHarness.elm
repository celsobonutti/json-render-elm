port module TestHarness exposing (main)

import Browser
import Components.Actions exposing (Action(..), decodeAction)
import Components.Registry exposing (registry)
import Html exposing (Html, div)
import Html.Attributes exposing (id)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Actions as Actions
import Random
import JsonRender.Render as Render
import JsonRender.Spec as Spec exposing (Spec)


port jsonRenderSpecIn : (Value -> msg) -> Sub msg


port jsonRenderStateIn : (Value -> msg) -> Sub msg


port testActionOut : Value -> Cmd msg


port testDecodeErrorOut : String -> Cmd msg


type alias Model =
    { spec : Maybe Spec
    , renderState : Value
    , seed : Random.Seed
    }


type Msg
    = SpecReceived Value
    | StateReceived Value
    | JsonRenderMsg (Actions.Msg Action)


init : Int -> ( Model, Cmd Msg )
init randomSeed =
    ( { spec = Nothing
      , renderState = Encode.object []
      , seed = Random.initialSeed randomSeed
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpecReceived val ->
            case Decode.decodeValue Spec.decoder val of
                Ok spec ->
                    ( { model
                        | spec = Just spec
                        , renderState = Maybe.withDefault model.renderState spec.state
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( model, testDecodeErrorOut (Decode.errorToString err) )

        StateReceived val ->
            ( { model | renderState = val }, Cmd.none )

        JsonRenderMsg actionMsg ->
            let
                actionsModel =
                    { spec = model.spec, state = model.renderState, seed = model.seed }

                ( newActionsModel, cmd ) =
                    Actions.update actionConfig actionMsg actionsModel
            in
            ( { model | renderState = newActionsModel.state, seed = newActionsModel.seed }
            , Cmd.map JsonRenderMsg cmd
            )


actionConfig : Actions.ActionConfig Action
actionConfig =
    { handleAction = handleAction
    , decodeAction = decodeAction
    , functions = registry.functions
    }


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


handleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )
handleAction action model =
    ( model, testActionOut (encodeAction action) )


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


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
