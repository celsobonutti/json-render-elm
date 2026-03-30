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
import JsonRender.Spec exposing (ActionBinding, EventHandler(..), Spec)
import JsonRender.State as State
import Random
import UUID


type alias Model =
    { spec : Maybe Spec
    , state : Value
    , seed : Random.Seed
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
    | WatcherTriggered EventHandler (Maybe RepeatContext)
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

        WatcherTriggered handler repeatCtx ->
            let
                bindings =
                    case handler of
                        SingleAction binding ->
                            [ binding ]

                        ChainedActions bs ->
                            bs
            in
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


{-| Walk a JSON Value, replacing every "$id" string with a fresh UUID.
Each "$id" gets a different UUID; the seed threads through.
-}
substituteIds : Random.Seed -> Value -> ( Value, Random.Seed )
substituteIds seed value =
    case Decode.decodeValue Decode.string value of
        Ok str ->
            if str == "$id" then
                let
                    ( uuid, newSeed ) =
                        Random.step UUID.generator seed
                in
                ( Json.Encode.string (UUID.toString uuid), newSeed )

            else
                ( value, seed )

        Err _ ->
            case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                Ok pairs ->
                    let
                        ( newPairs, newSeed ) =
                            List.foldl
                                (\( key, val ) ( accPairs, accSeed ) ->
                                    let
                                        ( newVal, nextSeed ) =
                                            substituteIds accSeed val
                                    in
                                    ( ( key, newVal ) :: accPairs, nextSeed )
                                )
                                ( [], seed )
                                pairs
                    in
                    ( Json.Encode.object (List.reverse newPairs), newSeed )

                Err _ ->
                    case Decode.decodeValue (Decode.list Decode.value) value of
                        Ok items ->
                            let
                                ( newItems, newSeed ) =
                                    List.foldl
                                        (\item ( accItems, accSeed ) ->
                                            let
                                                ( newItem, nextSeed ) =
                                                    substituteIds accSeed item
                                            in
                                            ( newItem :: accItems, nextSeed )
                                        )
                                        ( [], seed )
                                        items
                            in
                            ( Json.Encode.list identity (List.reverse newItems), newSeed )

                        Err _ ->
                            ( value, seed )


{-| Pre-process resolved action params before execution.
For pushState: substitute $id in the value param.
-}
preProcess : String -> Dict String Value -> Random.Seed -> ( Dict String Value, Random.Seed )
preProcess actionName resolvedParams seed =
    case ( actionName, Dict.get "value" resolvedParams ) of
        ( "pushState", Just value ) ->
            let
                ( newValue, newSeed ) =
                    substituteIds seed value
            in
            ( Dict.insert "value" newValue resolvedParams, newSeed )

        _ ->
            ( resolvedParams, seed )


{-| Post-process after action execution.
For pushState: clear the state path specified by clearStatePath.
-}
postProcess : String -> Dict String Value -> Model -> Model
postProcess actionName processedParams model =
    case ( actionName, Dict.get "clearStatePath" processedParams ) of
        ( "pushState", Just clearPathVal ) ->
            case Decode.decodeValue Decode.string clearPathVal of
                Ok path ->
                    { model | state = State.set path (Json.Encode.string "") model.state }

                Err _ ->
                    model

        _ ->
            model


{-| Execute a single action binding: resolve params, then dispatch built-in or custom.
-}
executeOneAction : ActionConfig action -> Maybe RepeatContext -> ActionBinding -> Model -> ( Model, Cmd (Msg action) )
executeOneAction config repeatCtx binding model =
    let
        resolvedParams =
            Resolve.resolveActionParamsWith config.functions model.state repeatCtx binding.params

        ( processedParams, newSeed ) =
            preProcess binding.action resolvedParams model.seed

        model_ =
            { model | seed = newSeed }
    in
    case binding.action of
        "setState" ->
            case ( getPathParam processedParams, Dict.get "value" processedParams ) of
                ( Just pathVal, Just value ) ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            ( { model_ | state = State.set path value model_.state }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model_, Cmd.none )

                _ ->
                    ( model_, Cmd.none )

        "pushState" ->
            case ( getPathParam processedParams, Dict.get "value" processedParams ) of
                ( Just pathVal, Just value ) ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            ( postProcess binding.action processedParams
                                { model_ | state = State.push path value model_.state }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model_, Cmd.none )

                _ ->
                    ( model_, Cmd.none )

        "removeState" ->
            case getPathParam processedParams of
                Just pathVal ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            let
                                fullPath =
                                    case Dict.get "index" processedParams of
                                        Just indexVal ->
                                            case Decode.decodeValue Decode.int indexVal of
                                                Ok idx ->
                                                    path ++ "/" ++ String.fromInt idx

                                                Err _ ->
                                                    path

                                        Nothing ->
                                            path
                            in
                            ( { model_ | state = State.remove fullPath model_.state }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model_, Cmd.none )

                Nothing ->
                    ( model_, Cmd.none )

        _ ->
            case config.decodeAction binding.action processedParams of
                Ok action ->
                    config.handleAction action model_

                Err _ ->
                    ( model_, Cmd.none )
