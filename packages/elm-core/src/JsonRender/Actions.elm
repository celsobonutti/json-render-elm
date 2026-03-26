module JsonRender.Actions exposing
    ( ActionConfig
    , Model
    , Msg(..)
    , checkWatchers
    , update
    )

{-| Built-in action handling for json-render specs.
-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Resolve as Resolve exposing (RepeatContext)
import JsonRender.Spec exposing (ActionBinding, EventHandler(..), Spec)
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
    , functions : Resolve.FunctionDict
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


{-| Look up a path param, accepting both "statePath" and "path" since the
json-render prompt uses both names depending on context.
-}
getPathParam : Dict String Value -> Maybe Value
getPathParam params =
    case Dict.get "statePath" params of
        Just val ->
            Just val

        Nothing ->
            Dict.get "path" params


{-| Execute a single action binding: resolve params, then dispatch built-in or custom.
-}
executeOneAction : ActionConfig action -> Maybe RepeatContext -> ActionBinding -> Model -> ( Model, Cmd (Msg action) )
executeOneAction config repeatCtx binding model =
    let
        resolvedParams =
            Resolve.resolveActionParamsWith config.functions model.state repeatCtx binding.params
    in
    case binding.action of
        "setState" ->
            case ( getPathParam resolvedParams, Dict.get "value" resolvedParams ) of
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
            case ( getPathParam resolvedParams, Dict.get "value" resolvedParams ) of
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
            case getPathParam resolvedParams of
                Just pathVal ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            let
                                fullPath =
                                    case Dict.get "index" resolvedParams of
                                        Just indexVal ->
                                            case Decode.decodeValue Decode.int indexVal of
                                                Ok idx ->
                                                    path ++ "/" ++ String.fromInt idx

                                                Err _ ->
                                                    path

                                        Nothing ->
                                            path
                            in
                            ( { model | state = State.remove fullPath model.state }
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


{-| Check all watchers in the spec after a state change. Compares old state vs
new state at each watched path. If any changed, executes the watcher's actions
and re-checks (up to 10 iterations to prevent infinite loops).
-}
checkWatchers : ActionConfig action -> Value -> Model -> ( Model, Cmd (Msg action) )
checkWatchers config oldState model =
    checkWatchersLoop config 10 oldState model


checkWatchersLoop : ActionConfig action -> Int -> Value -> Model -> ( Model, Cmd (Msg action) )
checkWatchersLoop config remaining oldState model =
    if remaining <= 0 then
        ( model, Cmd.none )

    else
        case model.spec of
            Nothing ->
                ( model, Cmd.none )

            Just spec ->
                let
                    triggered =
                        collectTriggeredHandlers oldState model.state spec
                in
                if List.isEmpty triggered then
                    ( model, Cmd.none )

                else
                    let
                        stateBeforeWatchers =
                            model.state

                        ( modelAfterWatchers, cmd ) =
                            executeHandlers config triggered model

                        ( finalModel, moreCmd ) =
                            checkWatchersLoop config (remaining - 1) stateBeforeWatchers modelAfterWatchers
                    in
                    ( finalModel, Cmd.batch [ cmd, moreCmd ] )


collectTriggeredHandlers : Value -> Value -> Spec -> List EventHandler
collectTriggeredHandlers oldState newState spec =
    Dict.foldl
        (\_ element acc ->
            Dict.foldl
                (\path handler innerAcc ->
                    if statePathChanged path oldState newState then
                        handler :: innerAcc

                    else
                        innerAcc
                )
                acc
                element.watch
        )
        []
        spec.elements


statePathChanged : String -> Value -> Value -> Bool
statePathChanged path oldState newState =
    let
        oldVal =
            State.get path oldState |> Maybe.map (Encode.encode 0)

        newVal =
            State.get path newState |> Maybe.map (Encode.encode 0)
    in
    oldVal /= newVal


executeHandlers : ActionConfig action -> List EventHandler -> Model -> ( Model, Cmd (Msg action) )
executeHandlers config handlers model =
    List.foldl
        (\handler ( accModel, accCmd ) ->
            let
                bindings =
                    case handler of
                        SingleAction binding ->
                            [ binding ]

                        ChainedActions bs ->
                            bs

                ( newModel, newCmd ) =
                    List.foldl
                        (\binding ( m, c ) ->
                            let
                                ( m2, c2 ) =
                                    executeOneAction config Nothing binding m
                            in
                            ( m2, Cmd.batch [ c, c2 ] )
                        )
                        ( accModel, Cmd.none )
                        bindings
            in
            ( newModel, Cmd.batch [ accCmd, newCmd ] )
        )
        ( model, Cmd.none )
        handlers
