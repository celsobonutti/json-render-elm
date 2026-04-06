module JsonRender.Render exposing
    ( Component
    , ComponentContext
    , RawComponentContext
    , Registry
    , register
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
import JsonRender.Internal.EventHandle as EventHandle exposing (EventHandle)
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
    { props : Dict String ResolvedValue
    , bindings : Dict String (Value -> EventHandle msg)
    , validation : Dict String Validation.FieldValidation
    , children : List (Html msg)
    , emit : String -> EventHandle msg
    , validate : EventHandle msg
    , validateAndEmit : String -> EventHandle msg
    , validateOn : Validation.ValidateOn
    }


type Component msg
    = Component (RawComponentContext msg -> Html msg)


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
    Component
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
                    Html.div
                        [ Html.Attributes.style "background" "#fee2e2"
                        , Html.Attributes.style "color" "#991b1b"
                        , Html.Attributes.style "padding" "8px 12px"
                        , Html.Attributes.style "border-radius" "4px"
                        , Html.Attributes.style "font-size" "13px"
                        , Html.Attributes.style "font-family" "monospace"
                        ]
                        [ Html.text ("Props error: " ++ err) ]
        )


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


findBindStatePath : Dict String PropValue -> Maybe String
findBindStatePath props =
    Dict.foldl
        (\_ propValue acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case propValue of
                        BindStateExpr path ->
                            Just path

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


renderChildren : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Maybe RepeatContext -> Spec -> List String -> List (Html (Msg action))
renderChildren registry state validationState repeatCtx spec childIds =
    List.filterMap
        (\id ->
            Dict.get id spec.elements
                |> Maybe.map (renderElement registry state validationState repeatCtx spec)
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


renderRepeatedChildren : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Spec -> Element -> Repeat -> List (Html (Msg action))
renderRepeatedChildren registry state validationState spec element repeat =
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
                                            |> Maybe.map (\el -> ( itemKey, renderElement registry state validationState ctx spec el ))
                                    )
                                    element.children

                            else
                                List.filterMap
                                    (\id ->
                                        Dict.get id spec.elements
                                            |> Maybe.map (\el -> ( id ++ "-" ++ String.fromInt i ++ "-" ++ itemKey, renderElement registry state validationState ctx spec el ))
                                    )
                                    element.children
                        )
                        items
                        |> List.concat
            in
            [ Html.Keyed.node "div" [ Html.Attributes.style "display" "contents" ] keyedChildren ]

        Nothing ->
            []


render : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Spec -> Html (Msg action)
render registry state validationState spec =
    case Dict.get spec.root spec.elements of
        Just element ->
            Html.node "jr-validation-root"
                [ Html.Attributes.style "display" "contents"
                , Html.Events.on "validation-register"
                    (Decode.map2 RegisterValidation
                        (Decode.at [ "detail", "path" ] Decode.string)
                        (Decode.at [ "detail", "config" ] (Decode.string |> Decode.andThen decodeConfigString))
                    )
                , Html.Events.on "validation-unregister"
                    (Decode.at [ "detail", "path" ] Decode.string
                        |> Decode.map UnregisterValidation
                    )
                ]
                [ renderElement registry state validationState Nothing spec element ]

        Nothing ->
            Html.text ""


renderElement : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Maybe RepeatContext -> Spec -> Element -> Html (Msg action)
renderElement registry state validationState repeatCtx spec element =
    case element.visible of
        Just condition ->
            case Visibility.evaluate state repeatCtx condition of
                Ok True ->
                    renderElementInner registry state validationState repeatCtx spec element

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
            renderElementInner registry state validationState repeatCtx spec element


renderElementInner : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Maybe RepeatContext -> Spec -> Element -> Html (Msg action)
renderElementInner registry state validationState repeatCtx spec element =
    case Dict.get element.type_ registry.components of
        Just (Component componentFn) ->
            let
                resolved =
                    Resolve.resolvePropsWith registry.functions state repeatCtx element.props

                bindings =
                    extractBindings repeatCtx element.props

                children =
                    case element.repeat of
                        Just repeat ->
                            renderRepeatedChildren registry state validationState spec element repeat

                        Nothing ->
                            renderChildren registry state validationState repeatCtx spec element.children

                bindPropName =
                    findBindPropName element.props

                bindStatePath =
                    findBindStatePath element.props

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
                    case Validation.extractValidation element.checks element.validateOn element.enabled element.props of
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
                                        (Encode.encode 0 (Validation.encodeValidationConfig config))
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

        Nothing ->
            Html.text ""


decodeConfigString : String -> Decode.Decoder Validation.ValidationConfig
decodeConfigString jsonStr =
    case Decode.decodeString Validation.configDecoder jsonStr of
        Ok config ->
            Decode.succeed config

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
