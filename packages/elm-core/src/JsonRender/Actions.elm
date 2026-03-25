module JsonRender.Actions exposing
    ( Msg(..)
    , Model
    , update
    )

{-| Built-in action handling for json-render specs.
-}

import Json.Encode exposing (Value)
import JsonRender.Spec exposing (Spec)
import JsonRender.State as State


type alias Model =
    { spec : Maybe Spec
    , state : Value
    }


type Msg action
    = SpecReceived Value
    | SetState String Value
    | PushState String Value
    | RemoveState String
    | CustomAction action


update : (action -> Model -> ( Model, Cmd (Msg action) )) -> Msg action -> Model -> ( Model, Cmd (Msg action) )
update handleAction msg model =
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

        CustomAction action ->
            handleAction action model
