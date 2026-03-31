module JsonRender.Actions exposing
    ( ActionConfig
    , Model
    , Msg(..)
    , ResolvedAction(..)
    , applyAction
    , executeHandler
    , resolveBinding
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
    }


{-| A resolved, typed action ready to be applied to the model.
-}
type ResolvedAction action
    = SetState { path : String, value : Value }
    | PushState { path : String, value : Value, clearPath : Maybe String }
    | RemoveState { path : String, index : Maybe Int }
    | CustomAction action


type Msg action
    = SpecReceived Value
    | ExecuteAction EventHandler (Maybe RepeatContext)
    | BindingUpdate String Value
    | ActionError String


update : Resolve.FunctionDict -> ActionConfig action -> Msg action -> Model -> ( Model, Cmd (Msg action) )
update functions config msg model =
    case msg of
        SpecReceived _ ->
            ( model, Cmd.none )

        BindingUpdate path value ->
            ( { model | state = State.set path value model.state }, Cmd.none )

        ExecuteAction handler repeatCtx ->
            executeHandler functions config handler repeatCtx model

        ActionError _ ->
            ( model, Cmd.none )


{-| Execute an EventHandler (single or chained actions) against the model.
-}
executeHandler : Resolve.FunctionDict -> ActionConfig action -> EventHandler -> Maybe RepeatContext -> Model -> ( Model, Cmd (Msg action) )
executeHandler functions config handler repeatCtx model =
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
            case resolveBinding functions config binding repeatCtx accModel of
                Ok ( resolved, newSeed ) ->
                    let
                        ( newModel, newCmd ) =
                            applyAction config resolved { accModel | seed = newSeed }
                    in
                    ( newModel, Cmd.batch [ accCmd, newCmd ] )

                Err _ ->
                    ( accModel, accCmd )
        )
        ( model, Cmd.none )
        bindings


getPathParam : Dict String Value -> Maybe Value
getPathParam params =
    Dict.get "statePath" params


{-| Walk a JSON Value, replacing every "$id" string with a fresh UUID.
Each "$id" gets a different UUID; the seed threads through.
-}
substituteIds : Random.Seed -> Value -> ( Value, Random.Seed )
substituteIds seed value =
    substituteIdsHelp seed value []


type SubFrame
    = SubListFrame (List Value) (List Value)
    | SubObjectFrame String (List ( String, Value )) (List ( String, Value ))


substituteIdsHelp : Random.Seed -> Value -> List SubFrame -> ( Value, Random.Seed )
substituteIdsHelp seed value stack =
    case Decode.decodeValue Decode.string value of
        Ok str ->
            if str == "$id" then
                let
                    ( uuid, newSeed ) =
                        Random.step UUID.generator seed
                in
                returnSubstituted newSeed (Json.Encode.string (UUID.toString uuid)) stack

            else
                returnSubstituted seed value stack

        Err _ ->
            case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                Ok pairs ->
                    case pairs of
                        [] ->
                            returnSubstituted seed (Json.Encode.object []) stack

                        ( k, v ) :: rest ->
                            substituteIdsHelp seed v (SubObjectFrame k rest [] :: stack)

                Err _ ->
                    case Decode.decodeValue (Decode.list Decode.value) value of
                        Ok items ->
                            case items of
                                [] ->
                                    returnSubstituted seed (Json.Encode.list identity []) stack

                                first :: rest ->
                                    substituteIdsHelp seed first (SubListFrame rest [] :: stack)

                        Err _ ->
                            returnSubstituted seed value stack


returnSubstituted : Random.Seed -> Value -> List SubFrame -> ( Value, Random.Seed )
returnSubstituted seed val stack =
    case stack of
        [] ->
            ( val, seed )

        (SubListFrame remaining done) :: rest ->
            let
                newDone =
                    val :: done
            in
            case remaining of
                [] ->
                    returnSubstituted seed (Json.Encode.list identity (List.reverse newDone)) rest

                next :: more ->
                    substituteIdsHelp seed next (SubListFrame more newDone :: rest)

        (SubObjectFrame key remaining done) :: rest ->
            let
                newDone =
                    ( key, val ) :: done
            in
            case remaining of
                [] ->
                    returnSubstituted seed (Json.Encode.object (List.reverse newDone)) rest

                ( nextKey, nextVal ) :: more ->
                    substituteIdsHelp seed nextVal (SubObjectFrame nextKey more newDone :: rest)


{-| Resolve an ActionBinding into a typed ResolvedAction.
Combines param resolution, $id substitution, and action decoding into one step.
-}
resolveBinding : Resolve.FunctionDict -> ActionConfig action -> ActionBinding -> Maybe RepeatContext -> Model -> Result String ( ResolvedAction action, Random.Seed )
resolveBinding functions config binding repeatCtx model =
    let
        resolvedParams =
            Resolve.resolveActionParamsWith functions model.state repeatCtx binding.params
    in
    case binding.action of
        "setState" ->
            case ( getPathParam resolvedParams, Dict.get "value" resolvedParams ) of
                ( Just pathVal, Just value ) ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            Ok ( SetState { path = path, value = value }, model.seed )

                        Err _ ->
                            Err "setState: statePath must be a string"

                _ ->
                    Err "setState: missing statePath or value"

        "pushState" ->
            case ( getPathParam resolvedParams, Dict.get "value" resolvedParams ) of
                ( Just pathVal, Just value ) ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            let
                                ( substitutedValue, newSeed ) =
                                    substituteIds model.seed value

                                clearPath =
                                    Dict.get "clearStatePath" resolvedParams
                                        |> Maybe.andThen (\v -> Decode.decodeValue Decode.string v |> Result.toMaybe)
                            in
                            Ok ( PushState { path = path, value = substitutedValue, clearPath = clearPath }, newSeed )

                        Err _ ->
                            Err "pushState: statePath must be a string"

                _ ->
                    Err "pushState: missing statePath or value"

        "removeState" ->
            case getPathParam resolvedParams of
                Just pathVal ->
                    case Decode.decodeValue Decode.string pathVal of
                        Ok path ->
                            let
                                index =
                                    Dict.get "index" resolvedParams
                                        |> Maybe.andThen (\v -> Decode.decodeValue Decode.int v |> Result.toMaybe)
                            in
                            Ok ( RemoveState { path = path, index = index }, model.seed )

                        Err _ ->
                            Err "removeState: statePath must be a string"

                Nothing ->
                    Err "removeState: missing statePath"

        _ ->
            case config.decodeAction binding.action resolvedParams of
                Ok action ->
                    Ok ( CustomAction action, model.seed )

                Err err ->
                    Err err


{-| Apply a ResolvedAction to the model.
-}
applyAction : ActionConfig action -> ResolvedAction action -> Model -> ( Model, Cmd (Msg action) )
applyAction config resolved model =
    case resolved of
        SetState { path, value } ->
            ( { model | state = State.set path value model.state }, Cmd.none )

        PushState { path, value, clearPath } ->
            let
                pushed =
                    { model | state = State.push path value model.state }
            in
            case clearPath of
                Just cp ->
                    ( { pushed | state = State.set cp (Json.Encode.string "") pushed.state }, Cmd.none )

                Nothing ->
                    ( pushed, Cmd.none )

        RemoveState { path, index } ->
            let
                fullPath =
                    case index of
                        Just idx ->
                            path ++ "/" ++ String.fromInt idx

                        Nothing ->
                            path
            in
            ( { model | state = State.remove fullPath model.state }, Cmd.none )

        CustomAction action ->
            config.handleAction action model
