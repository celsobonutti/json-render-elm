module JsonRender.Actions exposing
    ( Msg(..)
    , Model
    , update
    , encodeAction
    )

{-| Built-in action handling for json-render specs.
-}

import Json.Encode as Encode exposing (Value)
import JsonRender.Spec exposing (Spec)
import JsonRender.State as State


type alias Model =
    { spec : Maybe Spec
    , state : Value
    }


type Msg
    = SpecReceived Value
    | SetState String Value
    | PushState String Value
    | RemoveState String
    | CustomAction String Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SpecReceived _ ->
            ( model, Cmd.none )

        SetState path value ->
            ( { model | state = State.set path value model.state }
            , Cmd.none
            )

        PushState path value ->
            ( { model | state = State.push path value model.state }
            , Cmd.none
            )

        RemoveState path ->
            ( { model | state = State.remove path model.state }
            , Cmd.none
            )

        CustomAction _ _ ->
            ( model, Cmd.none )


encodeAction : String -> Value -> Value
encodeAction name params =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "params", params )
        ]
