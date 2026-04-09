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
import Json.Encode as Encode exposing (Value)
import JsonRender.Internal.ComponentInstance exposing (ComponentInstance)
import JsonRender.Internal.EventHandle exposing (EventHandle(..))
import JsonRender.Resolve as Resolve exposing (RepeatContext)
import JsonRender.Spec as Spec exposing (ActionBinding, EventHandler(..), Spec)
import JsonRender.State as State
import JsonRender.Validation as Validation
import Random
import Recursion
import Recursion.Fold
import UUID


type alias Model action =
    { spec : Maybe Spec
    , state : Value
    , seed : Random.Seed
    , validationState : Dict String Validation.FieldValidation
    , validationRegistry : Dict String ( Validation.ValidationConfig, Maybe RepeatContext )
    , localComponents : Dict String (ComponentInstance (Msg action))
    }


{-| Configuration for action dispatch. Bundles a handler for decoded custom
actions with a decoder that turns action name + resolved params into an action.
-}
type alias ActionConfig action =
    { handleAction : action -> Model action -> ( Model action, Cmd (Msg action) )
    , decodeAction : String -> Dict String Value -> Result String action
    }


{-| A resolved, typed action ready to be applied to the model.
-}
type ResolvedAction action
    = SetState { path : String, value : Value }
    | PushState { path : String, value : Value, clearPath : Maybe String }
    | RemoveState { path : String, index : Maybe Int }
    | ValidateForm { statePath : String }
    | CustomAction action


type Msg action
    = SpecReceived Value
    | ExecuteAction EventHandler (Maybe RepeatContext)
    | BindingUpdate String Value
    | ActionError String
    | ValidateField String
    | ValidateAndEmit String String (Maybe RepeatContext)
    | RegisterValidation String Validation.ValidationConfig (Maybe RepeatContext)
    | UnregisterValidation String
    | InitLocal String (ComponentInstance (Msg action))
    | UpdateLocal String (ComponentInstance (Msg action)) (List (EventHandle (Msg action)))


update : Resolve.FunctionDict -> ActionConfig action -> Msg action -> Model action -> ( Model action, Cmd (Msg action) )
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

        ValidateField path ->
            ( validateFieldHelper functions model path, Cmd.none )

        RegisterValidation path validationConfig repeatCtx ->
            ( { model | validationRegistry = Dict.insert path ( validationConfig, repeatCtx ) model.validationRegistry }, Cmd.none )

        UnregisterValidation path ->
            ( { model | validationRegistry = Dict.remove path model.validationRegistry }, Cmd.none )

        ValidateAndEmit path eventName repeatCtx ->
            let
                validatedModel =
                    validateFieldHelper functions model path
            in
            case validatedModel.spec of
                Just spec ->
                    case findElementByBindPath spec path of
                        Just element ->
                            case Dict.get eventName element.on of
                                Just handler ->
                                    executeHandler functions config handler repeatCtx validatedModel

                                Nothing ->
                                    ( validatedModel, Cmd.none )

                        Nothing ->
                            ( validatedModel, Cmd.none )

                Nothing ->
                    ( validatedModel, Cmd.none )

        InitLocal key instance ->
            ( { model | localComponents = Dict.insert key instance model.localComponents }
            , Cmd.none
            )

        UpdateLocal key newInstance handles ->
            let
                model_ =
                    { model | localComponents = Dict.insert key newInstance model.localComponents }
            in
            processHandles functions config handles model_


processHandles : Resolve.FunctionDict -> ActionConfig action -> List (EventHandle (Msg action)) -> Model action -> ( Model action, Cmd (Msg action) )
processHandles functions config handles model =
    List.foldl
        (\(EventHandle { message }) ( accModel, accCmd ) ->
            let
                ( m, c ) =
                    update functions config message accModel
            in
            ( m, Cmd.batch [ accCmd, c ] )
        )
        ( model, Cmd.none )
        handles


{-| Execute an EventHandler (single or chained actions) against the model.
-}
executeHandler : Resolve.FunctionDict -> ActionConfig action -> EventHandler -> Maybe RepeatContext -> Model action -> ( Model action, Cmd (Msg action) )
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
                            applyAction functions config resolved { accModel | seed = newSeed }
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
    Recursion.runRecursion substituteIdsStep ( seed, value )


substituteIdsStep : ( Random.Seed, Value ) -> Recursion.Rec ( Random.Seed, Value ) ( Value, Random.Seed ) ( Value, Random.Seed )
substituteIdsStep ( seed, value ) =
    case Decode.decodeValue Decode.string value of
        Ok str ->
            if str == "$id" then
                let
                    ( uuid, newSeed ) =
                        Random.step UUID.generator seed
                in
                Recursion.base ( Encode.string (UUID.toString uuid), newSeed )

            else
                Recursion.base ( value, seed )

        Err _ ->
            case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                Ok pairs ->
                    Recursion.Fold.foldMapList
                        (\( key, val ) ( accPairs, accSeed ) ->
                            Recursion.recurseThen ( accSeed, val ) <|
                                \( newVal, newSeed ) ->
                                    Recursion.base ( ( key, newVal ) :: accPairs, newSeed )
                        )
                        ( [], seed )
                        pairs
                        |> Recursion.map
                            (\( processedPairs, finalSeed ) ->
                                ( Encode.object (List.reverse processedPairs), finalSeed )
                            )

                Err _ ->
                    case Decode.decodeValue (Decode.list Decode.value) value of
                        Ok items ->
                            Recursion.Fold.foldMapList
                                (\item ( accItems, accSeed ) ->
                                    Recursion.recurseThen ( accSeed, item ) <|
                                        \( newItem, newSeed ) ->
                                            Recursion.base ( newItem :: accItems, newSeed )
                                )
                                ( [], seed )
                                items
                                |> Recursion.map
                                    (\( processedItems, finalSeed ) ->
                                        ( Encode.list identity (List.reverse processedItems), finalSeed )
                                    )

                        Err _ ->
                            Recursion.base ( value, seed )


{-| Resolve an ActionBinding into a typed ResolvedAction.
Combines param resolution, $id substitution, and action decoding into one step.
-}
resolveBinding : Resolve.FunctionDict -> ActionConfig action -> ActionBinding -> Maybe RepeatContext -> Model action -> Result String ( ResolvedAction action, Random.Seed )
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

        "validateForm" ->
            let
                statePath =
                    Dict.get "statePath" resolvedParams
                        |> Maybe.andThen (\v -> Decode.decodeValue Decode.string v |> Result.toMaybe)
                        |> Maybe.withDefault "/formValidation"
            in
            Ok ( ValidateForm { statePath = statePath }, model.seed )

        _ ->
            case config.decodeAction binding.action resolvedParams of
                Ok action ->
                    Ok ( CustomAction action, model.seed )

                Err err ->
                    Err err


{-| Apply a ResolvedAction to the model.
-}
applyAction : Resolve.FunctionDict -> ActionConfig action -> ResolvedAction action -> Model action -> ( Model action, Cmd (Msg action) )
applyAction functions config resolved model =
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
                    ( { pushed | state = State.set cp (Encode.string "") pushed.state }, Cmd.none )

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

        ValidateForm { statePath } ->
            let
                ( allValid, errorPairs, newValidationState ) =
                    Dict.foldl
                        (\path ( validationConfig, repeatCtx ) ( valid, errs, valState ) ->
                            let
                                fieldValue =
                                    State.get path model.state |> Maybe.withDefault Encode.null

                                result =
                                    Validation.runValidation Dict.empty functions validationConfig fieldValue model.state repeatCtx
                            in
                            ( valid && result.valid
                            , if result.valid then
                                errs

                              else
                                ( path, Encode.list Encode.string result.errors ) :: errs
                            , Dict.insert path { errors = result.errors, touched = True, validated = True } valState
                            )
                        )
                        ( True, [], model.validationState )
                        model.validationRegistry

                resultValue =
                    Encode.object
                        [ ( "valid", Encode.bool allValid )
                        , ( "errors", Encode.object errorPairs )
                        ]
            in
            ( { model
                | state = State.set statePath resultValue model.state
                , validationState = newValidationState
              }
            , Cmd.none
            )

        CustomAction action ->
            config.handleAction action model



-- Helpers


validateFieldHelper : Resolve.FunctionDict -> Model action -> String -> Model action
validateFieldHelper functions model path =
    let
        maybeEntry =
            Dict.get path model.validationRegistry

        fieldValue =
            State.get path model.state |> Maybe.withDefault Encode.null

        result =
            case maybeEntry of
                Just ( config_, repeatCtx ) ->
                    Validation.runValidation Dict.empty functions config_ fieldValue model.state repeatCtx

                Nothing ->
                    { valid = True, errors = [] }

        newValidationState =
            Dict.insert path
                { errors = result.errors, touched = True, validated = True }
                model.validationState
    in
    { model | validationState = newValidationState }


findElementByBindPath : Spec -> String -> Maybe Spec.Element
findElementByBindPath spec path =
    Dict.foldl
        (\_ element acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case Validation.extractValidation element.checks element.validateOn element.enabled element.props Nothing of
                        Just ( p, _ ) ->
                            if p == path then
                                Just element

                            else
                                Nothing

                        Nothing ->
                            Nothing
        )
        Nothing
        spec.elements
