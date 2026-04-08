port module Catalog.MiniApps.Input exposing (main)

import Browser
import Catalog.Components.Input as Input
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Resolve as Resolve
import JsonRender.Validation exposing (FieldValidation)


port propsIn : (Value -> msg) -> Sub msg


port actionsOut : Value -> Cmd msg


type alias Model =
    { props : Result String Input.InputProps
    , bindingPath : Maybe String
    , validation : Maybe FieldValidation
    , validateOn : String
    }


type Msg
    = PropsUpdated Value
    | UserInput String
    | UserBlur


decodeProps : Value -> Result String Input.InputProps
decodeProps flags =
    case Decode.decodeValue (Decode.field "props" Decode.value) flags of
        Ok propsValue ->
            case Decode.decodeValue (Decode.dict Resolve.valueDecoder) propsValue of
                Ok resolvedDict ->
                    Input.propsDecoder resolvedDict

                Err err ->
                    Err (Decode.errorToString err)

        Err err ->
            Err (Decode.errorToString err)


decodeBindingPath : Value -> Maybe String
decodeBindingPath flags =
    Decode.decodeValue
        (Decode.at [ "bindings", "value" ] Decode.string)
        flags
        |> Result.toMaybe


decodeValidation : Value -> Maybe FieldValidation
decodeValidation flags =
    Decode.decodeValue
        (Decode.at [ "validation", "value" ]
            (Decode.map3 FieldValidation
                (Decode.field "errors" (Decode.list Decode.string))
                (Decode.field "touched" Decode.bool)
                (Decode.field "validated" Decode.bool)
            )
        )
        flags
        |> Result.toMaybe


decodeValidateOn : Value -> String
decodeValidateOn flags =
    Decode.decodeValue (Decode.field "validateOn" Decode.string) flags
        |> Result.withDefault "submit"


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { props = decodeProps flags
      , bindingPath = decodeBindingPath flags
      , validation = decodeValidation flags
      , validateOn = decodeValidateOn flags
      }
    , actionsOut (Encode.object [])
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PropsUpdated flags ->
            ( { model
                | props = decodeProps flags
                , bindingPath = decodeBindingPath flags
                , validation = decodeValidation flags
                , validateOn = decodeValidateOn flags
              }
            , Cmd.none
            )

        UserInput newValue ->
            case model.bindingPath of
                Just path ->
                    ( { model
                        | props =
                            model.props
                                |> Result.map (\p -> { p | value = newValue })
                      }
                    , Cmd.batch
                        [ actionsOut
                            (Encode.object
                                [ ( "type", Encode.string "binding" )
                                , ( "path", Encode.string path )
                                , ( "value", Encode.string newValue )
                                ]
                            )
                        , if model.validateOn == "change" then
                            actionsOut
                                (Encode.object
                                    [ ( "type", Encode.string "validate" )
                                    , ( "path", Encode.string path )
                                    ]
                                )

                          else
                            Cmd.none
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )

        UserBlur ->
            let
                validateCmd =
                    if model.validateOn == "blur" then
                        case model.bindingPath of
                            Just path ->
                                actionsOut
                                    (Encode.object
                                        [ ( "type", Encode.string "validate" )
                                        , ( "path", Encode.string path )
                                        ]
                                    )

                            Nothing ->
                                Cmd.none

                    else
                        Cmd.none

                emitCmd =
                    actionsOut
                        (Encode.object
                            [ ( "type", Encode.string "emit" )
                            , ( "event", Encode.string "blur" )
                            ]
                        )
            in
            ( model, Cmd.batch [ validateCmd, emitCmd ] )


view : Model -> Html Msg
view model =
    case model.props of
        Ok props ->
            Input.viewStateless props
                { onInput =
                    if model.bindingPath /= Nothing then
                        Just UserInput

                    else
                        Nothing
                , onBlur = UserBlur
                , validation = model.validation
                }

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
