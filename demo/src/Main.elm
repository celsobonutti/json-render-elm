port module Main exposing (main)

import Browser
import Components.Actions exposing (Action(..), decodeAction)
import Components.Registry exposing (registry)
import Dict
import Html exposing (Html, button, div, h1, p, pre, span, text, textarea)
import Html.Attributes exposing (class, disabled, placeholder, rows, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Actions as Actions
import JsonRender.Render as Render
import JsonRender.Spec as Spec exposing (Spec)


-- App-specific ports
port sendPrompt : String -> Cmd msg


port receiveError : (String -> msg) -> Sub msg


port downloadJson : Value -> Cmd msg


-- json-render-elm bridge ports
port jsonRenderSpecIn : (Value -> msg) -> Sub msg



-- MODEL


type State
    = Idle
    | Loading
    | Rendered


type alias Model =
    { state : State
    , prompt : String
    , error : Maybe String
    , spec : Maybe Spec
    , specJson : Maybe Value
    , showJson : Bool
    , renderState : Value
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Idle
      , prompt = ""
      , error = Nothing
      , spec = Nothing
      , specJson = Nothing
      , showJson = False
      , renderState = Encode.object []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatePrompt String
    | Submit
    | SpecReceived Value
    | ErrorReceived String
    | Reset
    | ToggleJson
    | JsonRenderMsg (Actions.Msg Action)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatePrompt text ->
            ( { model | prompt = text }, Cmd.none )

        Submit ->
            if String.trim model.prompt == "" then
                ( model, Cmd.none )

            else
                ( { model | state = Loading, error = Nothing }
                , sendPrompt model.prompt
                )

        SpecReceived val ->
            case Decode.decodeValue Spec.decoder val of
                Ok spec ->
                    ( { model
                        | state = Rendered
                        , spec = Just spec
                        , specJson = Just val
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | state = Idle
                        , error = Just ("Failed to decode spec: " ++ Decode.errorToString err)
                      }
                    , Cmd.none
                    )

        ErrorReceived err ->
            ( { model | state = Idle, error = Just err }, Cmd.none )

        Reset ->
            ( { model
                | state = Idle
                , spec = Nothing
                , specJson = Nothing
                , showJson = False
                , prompt = ""
                , error = Nothing
                , renderState = Encode.object []
              }
            , Cmd.none
            )

        ToggleJson ->
            ( { model | showJson = not model.showJson }, Cmd.none )

        JsonRenderMsg actionMsg ->
            let
                actionsModel =
                    { spec = model.spec, state = model.renderState }

                ( newActionsModel, cmd ) =
                    Actions.update actionConfig actionMsg actionsModel
            in
            ( { model | renderState = newActionsModel.state }
            , Cmd.map JsonRenderMsg cmd
            )


actionConfig : Actions.ActionConfig Action
actionConfig =
    { handleAction = handleAction
    , decodeAction = decodeAction
    }


handleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )
handleAction action model =
    case action of
        Press ->
            ( model, Cmd.none )

        Export _ ->
            ( model, downloadJson model.state )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ viewHeader model
        , viewContent model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div [ class "header" ]
        [ h1 [] [ text "json-render-elm" ]
        , span [ class "header-subtitle" ] [ text "Generate UIs with natural language" ]
        , case model.state of
            Rendered ->
                button [ class "reset-button", onClick Reset ] [ text "New prompt" ]

            _ ->
                text ""
        ]


viewContent : Model -> Html Msg
viewContent model =
    case model.state of
        Idle ->
            viewPromptForm model

        Loading ->
            viewLoading model

        Rendered ->
            viewRendered model


viewPromptForm : Model -> Html Msg
viewPromptForm model =
    div [ class "prompt-container" ]
        [ case model.error of
            Just err ->
                div [ class "error" ] [ text err ]

            Nothing ->
                text ""
        , Html.form [ class "prompt-form", onSubmit Submit ]
            [ textarea
                [ class "prompt-input"
                , placeholder "Describe a UI... (e.g., 'A user profile card with name, email, and a settings button')"
                , value model.prompt
                , onInput UpdatePrompt
                , rows 3
                ]
                []
            , button
                [ class "submit-button"
                , disabled (String.trim model.prompt == "")
                ]
                [ text "Generate UI" ]
            ]
        ]


viewLoading : Model -> Html Msg
viewLoading model =
    div [ class "prompt-container" ]
        [ div [ class "prompt-form" ]
            [ textarea
                [ class "prompt-input"
                , value model.prompt
                , disabled True
                , rows 3
                ]
                []
            , button [ class "submit-button", disabled True ]
                [ text "Generating..." ]
            ]
        , div [ class "loading" ]
            [ div [ class "spinner" ] []
            , p [] [ text "Claude is generating your UI..." ]
            ]
        ]


viewRendered : Model -> Html Msg
viewRendered model =
    case model.spec of
        Just spec ->
            div [ class "rendered-container" ]
                [ div [ class "rendered-output" ]
                    [ Html.map JsonRenderMsg
                        (Render.render registry model.renderState spec)
                    ]
                , div [ class "json-panel" ]
                    [ button [ class "json-toggle", onClick ToggleJson ]
                        [ text
                            (if model.showJson then
                                "Hide JSON spec"

                             else
                                "Show JSON spec"
                            )
                        ]
                    , if model.showJson then
                        case model.specJson of
                            Just json ->
                                pre [ class "json-content" ]
                                    [ text (Encode.encode 2 json) ]

                            Nothing ->
                                text ""

                      else
                        text ""
                    ]
                ]

        Nothing ->
            text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsonRenderSpecIn SpecReceived
        , receiveError ErrorReceived
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
