port module Main exposing (main)

import Browser
import Components.Actions exposing (Action(..), decodeAction)
import Components.Registry exposing (registry)
import Html exposing (Html, button, div, h1, p, pre, span, text, textarea)
import Html.Attributes exposing (class, disabled, placeholder, rows, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender
import JsonRender.Actions as Actions
import Random



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
    , showJson : Bool
    , specJson : Maybe Value
    , jsonRender : JsonRender.Model
    }


init : Int -> ( Model, Cmd Msg )
init randomSeed =
    ( { state = Idle
      , prompt = ""
      , error = Nothing
      , showJson = False
      , specJson = Nothing
      , jsonRender = JsonRender.init (Random.initialSeed randomSeed)
      }
    , Cmd.none
    )


handleAction : Action -> Actions.Model -> ( Actions.Model, Cmd (Actions.Msg Action) )
handleAction action model =
    case action of
        Press ->
            ( model, Cmd.none )

        Export _ ->
            ( model, downloadJson model.state )


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
        }



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
            case JsonRender.receiveSpec val model.jsonRender of
                Ok jr ->
                    ( { model
                        | state = Rendered
                        , jsonRender = jr
                        , specJson = Just val
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | state = Idle
                        , error = Just ("Failed to decode spec: " ++ err)
                      }
                    , Cmd.none
                    )

        ErrorReceived err ->
            ( { model | state = Idle, error = Just err }, Cmd.none )

        Reset ->
            ( { model
                | state = Idle
                , jsonRender = JsonRender.init model.jsonRender.seed
                , specJson = Nothing
                , showJson = False
                , prompt = ""
                , error = Nothing
              }
            , Cmd.none
            )

        ToggleJson ->
            ( { model | showJson = not model.showJson }, Cmd.none )

        JsonRenderMsg actionMsg ->
            app.update actionMsg model



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
    case model.jsonRender.spec of
        Just _ ->
            div [ class "rendered-container" ]
                [ div [ class "rendered-output" ]
                    [ app.render model ]
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


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
