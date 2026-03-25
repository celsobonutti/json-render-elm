module JsonRender.Render exposing
    ( Component
    , Registry
    , ComponentContext
    , RawComponentContext
    , register
    , render
    )

{-| Spec-to-Html rendering with type-safe components.
-}

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Keyed
import Json.Decode as Decode
import Json.Encode exposing (Value)
import JsonRender.Actions exposing (Msg(..))
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Resolve as Resolve exposing (RepeatContext, ResolvedValue)
import JsonRender.Spec exposing (Element, Repeat, Spec)
import JsonRender.State as State
import JsonRender.Visibility as Visibility


type alias ComponentContext props bindings action =
    { props : props
    , bindings : bindings
    , children : List (Html (Msg action))
    , emit : action -> Msg action
    }


type alias RawComponentContext action =
    { props : Dict String ResolvedValue
    , bindings : Dict String (Value -> Msg action)
    , children : List (Html (Msg action))
    , emit : action -> Msg action
    }


type Component action
    = Component (RawComponentContext action -> Html (Msg action))


type alias Registry action =
    Dict String (Component action)


register :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> Msg action) -> bindings)
    -> (ComponentContext props bindings action -> Html (Msg action))
    -> Component action
register propsDecoder bindingsDecoder view =
    Component
        (\raw ->
            case propsDecoder raw.props of
                Ok typed ->
                    view
                        { props = typed
                        , bindings = bindingsDecoder raw.bindings
                        , children = raw.children
                        , emit = raw.emit
                        }

                Err _ ->
                    Html.text ""
        )


extractBindings : Maybe RepeatContext -> Dict String PropValue -> Dict String (Value -> Msg action)
extractBindings repeatCtx props =
    Dict.foldl
        (\key propValue acc ->
            case propValue of
                BindStateExpr path ->
                    Dict.insert key (\val -> SetState path val) acc

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
                            Dict.insert key (\val -> SetState path val) acc

                        Nothing ->
                            acc

                _ ->
                    acc
        )
        Dict.empty
        props


renderChildren : Registry action -> Value -> Maybe RepeatContext -> Spec -> List String -> List (Html (Msg action))
renderChildren registry state repeatCtx spec childIds =
    List.filterMap
        (\id ->
            Dict.get id spec.elements
                |> Maybe.map (renderElement registry state repeatCtx spec)
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


renderRepeatedChildren : Registry action -> Value -> Spec -> Element -> Repeat -> List (Html (Msg action))
renderRepeatedChildren registry state spec element repeat =
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
                                            |> Maybe.map (\el -> ( itemKey, renderElement registry state ctx spec el ))
                                    )
                                    element.children

                            else
                                List.filterMap
                                    (\id ->
                                        Dict.get id spec.elements
                                            |> Maybe.map (\el -> ( id ++ "-" ++ String.fromInt i ++ "-" ++ itemKey, renderElement registry state ctx spec el ))
                                    )
                                    element.children
                        )
                        items
                        |> List.concat
            in
            [ Html.Keyed.node "div" [ Html.Attributes.style "display" "contents" ] keyedChildren ]

        Nothing ->
            []


render : Registry action -> Value -> Spec -> Html (Msg action)
render registry state spec =
    case Dict.get spec.root spec.elements of
        Just element ->
            renderElement registry state Nothing spec element

        Nothing ->
            Html.text ""


renderElement : Registry action -> Value -> Maybe RepeatContext -> Spec -> Element -> Html (Msg action)
renderElement registry state repeatCtx spec element =
    case element.visible of
        Just condition ->
            if Visibility.evaluate state repeatCtx condition then
                renderElementInner registry state repeatCtx spec element

            else
                Html.text ""

        Nothing ->
            renderElementInner registry state repeatCtx spec element


renderElementInner : Registry action -> Value -> Maybe RepeatContext -> Spec -> Element -> Html (Msg action)
renderElementInner registry state repeatCtx spec element =
    case Dict.get element.type_ registry of
        Just (Component componentFn) ->
            let
                resolved =
                    Resolve.resolveProps state repeatCtx element.props

                bindings =
                    extractBindings repeatCtx element.props

                children =
                    case element.repeat of
                        Just repeat ->
                            renderRepeatedChildren registry state spec element repeat

                        Nothing ->
                            renderChildren registry state repeatCtx spec element.children
            in
            componentFn
                { props = resolved
                , bindings = bindings
                , children = children
                , emit = \action -> CustomAction action
                }

        Nothing ->
            Html.text ""
