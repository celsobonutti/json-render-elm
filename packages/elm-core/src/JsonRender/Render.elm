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
import Json.Encode exposing (Value)
import JsonRender.Actions exposing (Msg(..))
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Resolve as Resolve exposing (RepeatContext, ResolvedValue)
import JsonRender.Spec exposing (Element, Spec)
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


extractBindings : Dict String PropValue -> Dict String (Value -> Msg action)
extractBindings props =
    Dict.foldl
        (\key propValue acc ->
            case propValue of
                BindStateExpr path ->
                    Dict.insert key (\val -> SetState path val) acc

                _ ->
                    acc
        )
        Dict.empty
        props


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
                    extractBindings element.props

                children =
                    List.filterMap
                        (\id ->
                            Dict.get id spec.elements
                                |> Maybe.map (renderElement registry state repeatCtx spec)
                        )
                        element.children
            in
            componentFn
                { props = resolved
                , bindings = bindings
                , children = children
                , emit = \action -> CustomAction action
                }

        Nothing ->
            Html.text ""
