module JsonRender.Actions exposing
    ( ActionConfig
    , Model
    , Msg(..)
    , update
    )

{-| Built-in action handling for json-render specs.
-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode exposing (Value)
import JsonRender.Resolve as Resolve exposing (RepeatContext)
import JsonRender.Spec exposing (ActionBinding, Spec)
import JsonRender.State as State


type alias Model =
    { spec : Maybe Spec
    , state : Value
    }


{-| Configuration for action dispatch. Bundles a handler for decoded custom
actions with a decoder that turns action name + resolved params into an action.
-}
type alias ActionConfig action =
    { handleAction : action -> Model -> ( Model, Cmd (Msg action) )
    , decodeAction : String -> Dict String Value -> Result String action
    }


type Msg action
    = SpecReceived Value
    | SetState String Value
    | PushState String Value
    | RemoveState String
    | CustomAction action
    | ExecuteAction ActionBinding (Maybe RepeatContext)
    | ExecuteChain (List ActionBinding) (Maybe RepeatContext)
    | ActionError String


update : ActionConfig action -> Msg action -> Model -> ( Model, Cmd (Msg action) )
update config msg model =
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
            config.handleAction action model

        ExecuteAction binding repeatCtx ->
            executeOneAction config repeatCtx binding model

        ExecuteChain bindings repeatCtx ->
            List.foldl
                (\binding ( accModel, accCmd ) ->
                    let
                        ( newModel, newCmd ) =
                            executeOneAction config repeatCtx binding accModel
                    in
                    ( newModel, Cmd.batch [ accCmd, newCmd ] )
                )
                ( model, Cmd.none )
                bindings

        ActionError _ ->
            ( model, Cmd.none )


{-| Execute a single action binding: resolve params, then dispatch built-in or custom.
-}
executeOneAction : ActionConfig action -> Maybe RepeatContext -> ActionBinding -> Model -> ( Model, Cmd (Msg action) )
executeOneAction config repeatCtx binding model =
    let
        resolvedParams =
            Resolve.resolveActionParams model.state repeatCtx binding.params
    in
    case binding.action of
        "setState" ->
            case ( Dict.get "path" resolvedParams, Dict.get "value" resolvedParams ) of
                ( Just pathVal, Just value ) ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            ( { model | state = State.set path value model.state }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        "pushState" ->
            case ( Dict.get "path" resolvedParams, Dict.get "value" resolvedParams ) of
                ( Just pathVal, Just value ) ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            ( { model | state = State.push path value model.state }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        "removeState" ->
            case Dict.get "path" resolvedParams of
                Just pathVal ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            ( { model | state = State.remove path model.state }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            case config.decodeAction binding.action resolvedParams of
                Ok action ->
                    config.handleAction action model

                Err _ ->
                    ( model, Cmd.none )
