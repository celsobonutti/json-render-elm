module JsonRender.Render exposing
    ( Component(..)
    , ComponentContext
    , RawComponentContext
    , Registry
    , register
    , registerStateful
    , render
    )

{-| Spec-to-Html rendering with type-safe components.
-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Actions exposing (Msg(..))
import JsonRender.Internal.ComponentInstance as ComponentInstance exposing (ComponentInstance(..))
import JsonRender.Internal.Effect exposing (Effect(..), EffectResult(..))
import JsonRender.Internal.EventHandle as EventHandle exposing (EventHandle)
import JsonRender.Internal.PortCmd as PortCmd
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Resolve as Resolve exposing (RepeatContext, ResolvedValue)
import JsonRender.Spec exposing (Element, EventHandler, Repeat, Spec, shouldPreventDefault)
import JsonRender.State as State
import JsonRender.Validation as Validation
import JsonRender.Visibility as Visibility


type alias ComponentContext props bindings validation msg =
    { props : props
    , bindings : bindings
    , validation : validation
    , children : List (Html msg)
    , emit : String -> EventHandle msg
    , validate : EventHandle msg
    , validateAndEmit : String -> EventHandle msg
    , validateOn : Validation.ValidateOn
    }


type alias RawComponentContext msg =
    ComponentInstance.RawComponentContext msg


type Component msg
    = Stateless (RawComponentContext msg -> Html msg)
    | Stateful
        { create : String -> RawComponentContext msg -> ( ComponentInstance msg, Html msg )
        }


type alias Registry msg =
    { components : Dict String (Component msg)
    , functions : Resolve.FunctionDict
    }


register :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> EventHandle msg) -> bindings)
    -> (Dict String Validation.FieldValidation -> validation)
    -> (ComponentContext props bindings validation msg -> Html msg)
    -> Component msg
register propsDecoder bindingsDecoder validationDecoder view =
    Stateless
        (\raw ->
            case propsDecoder raw.props of
                Ok typed ->
                    view
                        { props = typed
                        , bindings = bindingsDecoder raw.bindings
                        , validation = validationDecoder raw.validation
                        , children = raw.children
                        , emit = raw.emit
                        , validate = raw.validate
                        , validateAndEmit = raw.validateAndEmit
                        , validateOn = raw.validateOn
                        }

                Err err ->
                    propsErrorHtml err
        )


registerStateful :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> EventHandle (Msg action)) -> bindings)
    -> (Dict String Validation.FieldValidation -> validation)
    -> { init : props -> state
       , update : localMsg -> state -> ComponentContext props bindings validation (Msg action) -> ( state, List (Effect (Msg action) localMsg) )
       , view : state -> props -> (localMsg -> Msg action) -> List (Html (Msg action)) -> Html (Msg action)
       , onPropsChange : Maybe (props -> state -> ( state, List (Effect (Msg action) localMsg) ))
       , portSubscriptions : List ( String, Value -> localMsg )
       }
    -> Component (Msg action)
registerStateful propsDecoder bindingsDecoder validationDecoder def =
    Stateful
        { create =
            \key raw ->
                case propsDecoder raw.props of
                    Ok props ->
                        let
                            state =
                                def.init props

                            instance =
                                buildInstance propsDecoder bindingsDecoder validationDecoder def key state raw
                        in
                        ( instance, viewInstance instance raw )

                    Err err ->
                        ( buildErrorInstance err
                        , propsErrorHtml err
                        )
        }


buildInstance :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> EventHandle (Msg action)) -> bindings)
    -> (Dict String Validation.FieldValidation -> validation)
    -> { init : props -> state
       , update : localMsg -> state -> ComponentContext props bindings validation (Msg action) -> ( state, List (Effect (Msg action) localMsg) )
       , view : state -> props -> (localMsg -> Msg action) -> List (Html (Msg action)) -> Html (Msg action)
       , onPropsChange : Maybe (props -> state -> ( state, List (Effect (Msg action) localMsg) ))
       , portSubscriptions : List ( String, Value -> localMsg )
       }
    -> String
    -> state
    -> RawComponentContext (Msg action)
    -> ComponentInstance (Msg action)
buildInstance propsDecoder bindingsDecoder validationDecoder def key state cachedRaw =
    -- elm-review: IGNORE TCO
    -- buildInstance returns immediately. The self-references are lambda captures, not calls —
    -- they only execute when the Elm runtime fires an event, in a separate call stack.
    let
        buildCtx raw =
            case propsDecoder raw.props of
                Ok props ->
                    Ok
                        { props = props
                        , bindings = bindingsDecoder raw.bindings
                        , validation = validationDecoder raw.validation
                        , children = raw.children
                        , emit = raw.emit
                        , validate = raw.validate
                        , validateAndEmit = raw.validateAndEmit
                        , validateOn = raw.validateOn
                        }

                Err err ->
                    Err err

        makeToMsg raw localMsg =
            -- elm-review: IGNORE TCO
            -- makeToMsg is guarded recursion: the self-reference in processEffects is a partial
            -- application passed to Cmd.map, only called asynchronously by the Elm runtime.
            case propsDecoder raw.props of
                Ok props ->
                    let
                        ctx =
                            { props = props
                            , bindings = bindingsDecoder raw.bindings
                            , validation = validationDecoder raw.validation
                            , children = raw.children
                            , emit = raw.emit
                            , validate = raw.validate
                            , validateAndEmit = raw.validateAndEmit
                            , validateOn = raw.validateOn
                            }

                        ( newState, effects ) =
                            def.update localMsg state ctx

                        newInstance =
                            buildInstance propsDecoder bindingsDecoder validationDecoder def key newState raw
                    in
                    UpdateLocal key newInstance (processEffects (makeToMsg raw) effects)

                Err _ ->
                    ActionError "Props decode failed during update"
    in
    ComponentInstance
        { view =
            \raw ->
                case buildCtx raw of
                    Ok ctx ->
                        def.view state ctx.props (makeToMsg raw) raw.children

                    Err err ->
                        propsErrorHtml err
        , onPropsChanged =
            \raw ->
                case def.onPropsChange of
                    Nothing ->
                        ( buildInstance propsDecoder bindingsDecoder validationDecoder def key state raw, [] )

                    Just handler ->
                        case propsDecoder raw.props of
                            Ok props ->
                                let
                                    ( newState, effects ) =
                                        handler props state

                                    newInstance =
                                        buildInstance propsDecoder bindingsDecoder validationDecoder def key newState raw
                                in
                                ( newInstance, processEffects (makeToMsg raw) effects )

                            Err _ ->
                                ( buildInstance propsDecoder bindingsDecoder validationDecoder def key state raw, [] )
        , handlePortIn =
            \portName value ->
                case findPortSubscription portName def.portSubscriptions of
                    Just portDecoder ->
                        case buildCtx cachedRaw of
                            Ok ctx ->
                                let
                                    localMsg =
                                        portDecoder value

                                    ( newState, effects ) =
                                        def.update localMsg state ctx

                                    newInstance =
                                        buildInstance propsDecoder bindingsDecoder validationDecoder def key newState cachedRaw
                                in
                                Just ( newInstance, processEffects (makeToMsg cachedRaw) effects )

                            Err _ ->
                                Nothing

                    Nothing ->
                        Nothing
        }


processEffects :
    (localMsg -> Msg action)
    -> List (Effect (Msg action) localMsg)
    -> List (EffectResult (Msg action))
processEffects toMsg effects =
    List.map
        (\effect ->
            case effect of
                Emit handle ->
                    EmitResult handle

                SendPort portName value ->
                    SendPortResult (PortCmd.portCmd portName value)

                RunCmd cmd ->
                    RunCmdResult (Cmd.map toMsg cmd)
        )
        effects


findPortSubscription : String -> List ( String, Value -> localMsg ) -> Maybe (Value -> localMsg)
findPortSubscription name subs =
    case subs of
        [] ->
            Nothing

        ( n, decoder ) :: rest ->
            if n == name then
                Just decoder

            else
                findPortSubscription name rest


viewInstance : ComponentInstance msg -> RawComponentContext msg -> Html msg
viewInstance (ComponentInstance inst) raw =
    inst.view raw


buildErrorInstance : String -> ComponentInstance msg
buildErrorInstance err =
    -- elm-review: IGNORE TCO
    -- buildErrorInstance returns immediately. The self-reference is a lambda capture, not a call —
    -- it only executes when the Elm runtime fires a props-changed event, in a separate call stack.
    ComponentInstance
        { view = \_ -> propsErrorHtml err
        , onPropsChanged = \_ -> ( buildErrorInstance err, [] )
        , handlePortIn = \_ _ -> Nothing
        }


propsErrorHtml : String -> Html msg
propsErrorHtml err =
    Html.div
        [ Html.Attributes.style "background" "#fee2e2"
        , Html.Attributes.style "color" "#991b1b"
        , Html.Attributes.style "padding" "8px 12px"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "font-size" "13px"
        , Html.Attributes.style "font-family" "monospace"
        ]
        [ Html.text ("Props error: " ++ err) ]


localStateKey : String -> Maybe RepeatContext -> String
localStateKey elementId repeatCtx =
    case repeatCtx of
        Just ctx ->
            elementId ++ ":" ++ ctx.basePath

        Nothing ->
            elementId


extractBindings : Maybe RepeatContext -> Dict String PropValue -> Dict String (Value -> EventHandle (Msg action))
extractBindings repeatCtx props =
    Dict.foldl
        (\key propValue acc ->
            case propValue of
                BindStateExpr path ->
                    Dict.insert key (\val -> EventHandle.fromMsg (BindingUpdate path val)) acc

                BindItemExpr field ->
                    case repeatCtx of
                        Just ctx ->
                            let
                                path =
                                    if field == "" then
                                        ctx.basePath

                                    else
                                        ctx.basePath ++ "/" ++ field
                            in
                            Dict.insert key (\val -> EventHandle.fromMsg (BindingUpdate path val)) acc

                        Nothing ->
                            acc

                _ ->
                    acc
        )
        Dict.empty
        props


findBindPropName : Dict String PropValue -> Maybe String
findBindPropName props =
    Dict.foldl
        (\key propValue acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case propValue of
                        BindStateExpr _ ->
                            Just key

                        BindItemExpr _ ->
                            Just key

                        _ ->
                            Nothing
        )
        Nothing
        props


findBindStatePath : Dict String PropValue -> Maybe RepeatContext -> Maybe String
findBindStatePath props repeatCtx =
    Dict.foldl
        (\_ propValue acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case propValue of
                        BindStateExpr path ->
                            Just path

                        BindItemExpr field ->
                            case repeatCtx of
                                Just ctx ->
                                    if field == "" then
                                        Just ctx.basePath

                                    else
                                        Just (ctx.basePath ++ "/" ++ field)

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing
        )
        Nothing
        props


{-| Build an emit function from an element's `on` dict and the current repeat context.
Looks up the event name in the `on` dict and produces the appropriate Msg.
-}
buildEmit : Dict String EventHandler -> Maybe RepeatContext -> String -> EventHandle (Msg action)
buildEmit onHandlers repeatCtx eventName =
    case Dict.get eventName onHandlers of
        Just handler ->
            EventHandle.withPreventDefault
                (shouldPreventDefault handler)
                (ExecuteAction handler repeatCtx)

        Nothing ->
            EventHandle.fromMsg (ActionError ("No handler for event: " ++ eventName))


renderChildren : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Dict String (ComponentInstance (Msg action)) -> Maybe RepeatContext -> Spec -> List String -> List (Html (Msg action))
renderChildren registry state validationState localComponents repeatCtx spec childIds =
    List.filterMap
        (\id ->
            Dict.get id spec.elements
                |> Maybe.map (renderElement registry state validationState localComponents repeatCtx spec id)
        )
        childIds


decodeList : Value -> Maybe (List Value)
decodeList value =
    Decode.decodeValue (Decode.list Decode.value) value |> Result.toMaybe


getItemKey : Maybe String -> Int -> Value -> String
getItemKey maybeKey index item =
    case maybeKey of
        Just keyField ->
            case Decode.decodeValue (Decode.field keyField Decode.string) item of
                Ok k ->
                    k

                Err _ ->
                    case Decode.decodeValue (Decode.field keyField Decode.int) item of
                        Ok k ->
                            String.fromInt k

                        Err _ ->
                            String.fromInt index

        Nothing ->
            String.fromInt index


renderRepeatedChildren : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Dict String (ComponentInstance (Msg action)) -> Spec -> Element -> Repeat -> List (Html (Msg action))
renderRepeatedChildren registry state validationState localComponents spec element repeat =
    case State.get repeat.statePath state |> Maybe.andThen decodeList of
        Just items ->
            let
                singleChild =
                    List.length element.children == 1

                keyedChildren =
                    List.indexedMap
                        (\i item ->
                            let
                                ctx =
                                    Just
                                        { item = item
                                        , index = i
                                        , basePath = repeat.statePath ++ "/" ++ String.fromInt i
                                        }

                                itemKey =
                                    getItemKey repeat.key i item
                            in
                            if singleChild then
                                List.filterMap
                                    (\id ->
                                        Dict.get id spec.elements
                                            |> Maybe.map (\el -> ( itemKey, renderElement registry state validationState localComponents ctx spec id el ))
                                    )
                                    element.children

                            else
                                List.filterMap
                                    (\id ->
                                        Dict.get id spec.elements
                                            |> Maybe.map (\el -> ( id ++ "-" ++ String.fromInt i ++ "-" ++ itemKey, renderElement registry state validationState localComponents ctx spec id el ))
                                    )
                                    element.children
                        )
                        items
                        |> List.concat
            in
            [ Html.Keyed.node "div" [ Html.Attributes.style "display" "contents" ] keyedChildren ]

        Nothing ->
            []


render : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Dict String (ComponentInstance (Msg action)) -> Spec -> Html (Msg action)
render registry state validationState localComponents spec =
    case Dict.get spec.root spec.elements of
        Just element ->
            Html.node "jr-validation-root"
                [ Html.Attributes.style "display" "contents"
                , Html.Events.on "validation-register"
                    (Decode.at [ "detail", "config" ] (Decode.string |> Decode.andThen decodeConfigString)
                        |> Decode.andThen
                            (\( config, repeatCtx ) ->
                                Decode.at [ "detail", "path" ] Decode.string
                                    |> Decode.map (\path -> RegisterValidation path config repeatCtx)
                            )
                    )
                , Html.Events.on "validation-unregister"
                    (Decode.at [ "detail", "path" ] Decode.string
                        |> Decode.map UnregisterValidation
                    )
                ]
                [ renderElement registry state validationState localComponents Nothing spec spec.root element ]

        Nothing ->
            Html.text ""


renderElement : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Dict String (ComponentInstance (Msg action)) -> Maybe RepeatContext -> Spec -> String -> Element -> Html (Msg action)
renderElement registry state validationState localComponents repeatCtx spec elementId element =
    case element.visible of
        Just condition ->
            case Visibility.evaluate state repeatCtx condition of
                Ok True ->
                    renderElementInner registry state validationState localComponents repeatCtx spec elementId element

                Ok False ->
                    Html.text ""

                Err err ->
                    Html.div
                        [ Html.Attributes.style "background" "#fee2e2"
                        , Html.Attributes.style "color" "#991b1b"
                        , Html.Attributes.style "padding" "8px 12px"
                        , Html.Attributes.style "border-radius" "4px"
                        , Html.Attributes.style "font-size" "13px"
                        , Html.Attributes.style "font-family" "monospace"
                        ]
                        [ Html.text ("Visibility error: " ++ err) ]

        Nothing ->
            renderElementInner registry state validationState localComponents repeatCtx spec elementId element


renderElementInner : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Dict String (ComponentInstance (Msg action)) -> Maybe RepeatContext -> Spec -> String -> Element -> Html (Msg action)
renderElementInner registry state validationState localComponents repeatCtx spec elementId element =
    case Dict.get element.type_ registry.components of
        Just (Stateless componentFn) ->
            let
                resolved =
                    Resolve.resolvePropsWith registry.functions state repeatCtx element.props

                bindings =
                    extractBindings repeatCtx element.props

                children =
                    case element.repeat of
                        Just repeat ->
                            renderRepeatedChildren registry state validationState localComponents spec element repeat

                        Nothing ->
                            renderChildren registry state validationState localComponents repeatCtx spec element.children

                bindPropName =
                    findBindPropName element.props

                bindStatePath =
                    findBindStatePath element.props repeatCtx

                fieldValidation =
                    case ( bindPropName, bindStatePath ) of
                        ( Just propName, Just path ) ->
                            case Dict.get path validationState of
                                Just fv ->
                                    Dict.singleton propName fv

                                Nothing ->
                                    Dict.empty

                        _ ->
                            Dict.empty

                validateHandle =
                    case bindStatePath of
                        Just path ->
                            EventHandle.fromMsg (ValidateField path)

                        Nothing ->
                            EventHandle.fromMsg (ActionError "No validation config")

                validateAndEmitHandle eventName =
                    case bindStatePath of
                        Just path ->
                            EventHandle.fromMsg (ValidateAndEmit path eventName repeatCtx)

                        Nothing ->
                            buildEmit element.on repeatCtx eventName

                componentHtml =
                    componentFn
                        { props = resolved
                        , bindings = bindings
                        , validation = fieldValidation
                        , children = children
                        , emit = buildEmit element.on repeatCtx
                        , validate = validateHandle
                        , validateAndEmit = validateAndEmitHandle
                        , validateOn = element.validateOn
                        }

                watcherTriggers =
                    renderWatcherTriggers state repeatCtx element.watch

                validationFields =
                    case Validation.extractValidation element.checks element.validateOn element.enabled element.props repeatCtx of
                        Just ( path, config ) ->
                            let
                                isEnabled =
                                    case config.enabled of
                                        Just condition ->
                                            Visibility.evaluate state repeatCtx condition
                                                |> Result.withDefault True

                                        Nothing ->
                                            True
                            in
                            if isEnabled then
                                [ Html.node "validation-field"
                                    [ Html.Attributes.attribute "data-path" path
                                    , Html.Attributes.attribute "data-config"
                                        (Encode.encode 0 (Validation.encodeValidationConfig config repeatCtx))
                                    ]
                                    []
                                ]

                            else
                                []

                        Nothing ->
                            []

                siblings =
                    watcherTriggers ++ validationFields
            in
            if List.isEmpty siblings then
                componentHtml

            else
                Html.div [ Html.Attributes.style "display" "contents" ]
                    (componentHtml :: siblings)

        Just (Stateful { create }) ->
            let
                key =
                    localStateKey elementId repeatCtx

                resolved =
                    Resolve.resolvePropsWith registry.functions state repeatCtx element.props

                bindings =
                    extractBindings repeatCtx element.props

                children =
                    case element.repeat of
                        Just repeat ->
                            renderRepeatedChildren registry state validationState localComponents spec element repeat

                        Nothing ->
                            renderChildren registry state validationState localComponents repeatCtx spec element.children

                bindPropName =
                    findBindPropName element.props

                bindStatePath =
                    findBindStatePath element.props repeatCtx

                fieldValidation =
                    case ( bindPropName, bindStatePath ) of
                        ( Just propName, Just path ) ->
                            case Dict.get path validationState of
                                Just fv ->
                                    Dict.singleton propName fv

                                Nothing ->
                                    Dict.empty

                        _ ->
                            Dict.empty

                validateHandle =
                    case bindStatePath of
                        Just path ->
                            EventHandle.fromMsg (ValidateField path)

                        Nothing ->
                            EventHandle.fromMsg (ActionError "No validation config")

                validateAndEmitHandle eventName =
                    case bindStatePath of
                        Just path ->
                            EventHandle.fromMsg (ValidateAndEmit path eventName repeatCtx)

                        Nothing ->
                            buildEmit element.on repeatCtx eventName

                rawCtx =
                    { props = resolved
                    , bindings = bindings
                    , validation = fieldValidation
                    , children = children
                    , emit = buildEmit element.on repeatCtx
                    , validate = validateHandle
                    , validateAndEmit = validateAndEmitHandle
                    , validateOn = element.validateOn
                    }

                watcherTriggers =
                    renderWatcherTriggers state repeatCtx element.watch

                validationFields =
                    case Validation.extractValidation element.checks element.validateOn element.enabled element.props repeatCtx of
                        Just ( path, config ) ->
                            let
                                isEnabled =
                                    case config.enabled of
                                        Just condition ->
                                            Visibility.evaluate state repeatCtx condition
                                                |> Result.withDefault True

                                        Nothing ->
                                            True
                            in
                            if isEnabled then
                                [ Html.node "validation-field"
                                    [ Html.Attributes.attribute "data-path" path
                                    , Html.Attributes.attribute "data-config"
                                        (Encode.encode 0 (Validation.encodeValidationConfig config repeatCtx))
                                    ]
                                    []
                                ]

                            else
                                []

                        Nothing ->
                            []

                propsJson =
                    resolved
                        |> Dict.map (\_ v -> Resolve.resolvedToValue v)
                        |> Dict.toList
                        |> Encode.object
                        |> Encode.encode 0

                propsChangedHandler inst =
                    let
                        ( newInstance, effects ) =
                            inst.onPropsChanged rawCtx
                    in
                    UpdateLocal key newInstance effects

                componentHtml =
                    case Dict.get key localComponents of
                        Just (ComponentInstance inst) ->
                            Html.node "component-mount"
                                [ Html.Attributes.attribute "data-props" propsJson
                                , Html.Attributes.attribute "data-instance" key
                                , Html.Attributes.attribute "data-component-type" element.type_
                                , Html.Events.on "props-changed"
                                    (Decode.succeed (propsChangedHandler inst))
                                ]
                                [ inst.view rawCtx ]

                        Nothing ->
                            let
                                ( instance, html ) =
                                    create key rawCtx

                                (ComponentInstance instRecord) =
                                    instance
                            in
                            Html.node "component-mount"
                                [ Html.Attributes.attribute "data-props" propsJson
                                , Html.Attributes.attribute "data-instance" key
                                , Html.Attributes.attribute "data-component-type" element.type_
                                , Html.Events.on "component-mounted"
                                    (Decode.succeed (InitLocal key instance))
                                , Html.Events.on "props-changed"
                                    (Decode.succeed (propsChangedHandler instRecord))
                                ]
                                [ html ]

                siblings =
                    watcherTriggers ++ validationFields
            in
            if List.isEmpty siblings then
                componentHtml

            else
                Html.div [ Html.Attributes.style "display" "contents" ]
                    (componentHtml :: siblings)

        Nothing ->
            Html.text ""


decodeConfigString : String -> Decode.Decoder ( Validation.ValidationConfig, Maybe RepeatContext )
decodeConfigString jsonStr =
    case Decode.decodeString Validation.configDecoder jsonStr of
        Ok result ->
            Decode.succeed result

        Err err ->
            Decode.fail (Decode.errorToString err)


renderWatcherTriggers : Value -> Maybe RepeatContext -> Dict String EventHandler -> List (Html (Msg action))
renderWatcherTriggers state repeatCtx watchDict =
    Dict.foldl
        (\path handler acc ->
            let
                watchedValue =
                    State.get path state
                        |> Maybe.map (Encode.encode 0)
                        |> Maybe.withDefault "null"
            in
            Html.node "watcher-trigger"
                [ Html.Attributes.attribute "value" watchedValue
                , Html.Events.on "watcher-triggered"
                    (Decode.succeed (ExecuteAction handler repeatCtx))
                ]
                []
                :: acc
        )
        []
        watchDict
