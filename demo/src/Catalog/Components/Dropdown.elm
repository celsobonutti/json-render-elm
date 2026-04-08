module Catalog.Components.Dropdown exposing
    ( DropdownBindings
    , DropdownOption
    , DropdownProps
    , LocalModel
    , LocalMsg(..)
    , bindingsDecoder
    , component
    , initLocal
    , propsDecoder
    , updateLocal
    , viewDropdown
    )

import Dict exposing (Dict)
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events
import Json.Decode as Decode
import Json.Encode exposing (Value)
import JsonRender.Bind as Bind
import JsonRender.Events exposing (EventHandle)
import JsonRender.Render exposing (Component, ComponentContext, registerStateful)
import JsonRender.Resolve as Resolve exposing (ResolvedValue(..))


type alias DropdownOption =
    { label : String, value : String }


type alias DropdownProps =
    { options : List DropdownOption
    , value : Maybe String
    , placeholder : Maybe String
    , label : Maybe String
    }


type alias DropdownBindings msg =
    { value : Maybe (String -> EventHandle msg) }


type alias LocalModel =
    { open : Bool
    , searchText : String
    , highlightedIndex : Int
    }


type LocalMsg
    = ToggleOpen
    | CloseDropdown
    | SetSearch String
    | HighlightIndex Int
    | SelectOption DropdownOption
    | KeyDown String


initLocal : LocalModel
initLocal =
    { open = False, searchText = "", highlightedIndex = 0 }


updateLocal : LocalMsg -> LocalModel -> LocalModel
updateLocal msg model =
    case msg of
        ToggleOpen ->
            { model | open = not model.open, searchText = "", highlightedIndex = 0 }

        CloseDropdown ->
            { model | open = False, searchText = "", highlightedIndex = 0 }

        SetSearch txt ->
            { model | searchText = txt, highlightedIndex = 0 }

        HighlightIndex i ->
            { model | highlightedIndex = i }

        SelectOption _ ->
            { model | open = False, searchText = "", highlightedIndex = 0 }

        KeyDown _ ->
            model


optionDecoder : ResolvedValue -> Result String DropdownOption
optionDecoder rv =
    case rv of
        RObject obj ->
            Result.map2 DropdownOption
                (Dict.get "label" obj
                    |> Maybe.map Resolve.string
                    |> Maybe.withDefault (Err "missing label")
                )
                (Dict.get "value" obj
                    |> Maybe.map Resolve.string
                    |> Maybe.withDefault (Err "missing value")
                )

        _ ->
            Err "expected object for option"


optionsDecoder : ResolvedValue -> Result String (List DropdownOption)
optionsDecoder rv =
    case rv of
        RList items ->
            items
                |> List.map optionDecoder
                |> List.foldr
                    (\r acc ->
                        case ( r, acc ) of
                            ( Ok item, Ok list ) ->
                                Ok (item :: list)

                            ( Err e, _ ) ->
                                Err e

                            ( _, Err e ) ->
                                Err e
                    )
                    (Ok [])

        _ ->
            Err "expected list for options"


propsDecoder : Dict String ResolvedValue -> Result String DropdownProps
propsDecoder =
    Resolve.succeed DropdownProps
        |> Resolve.required "options" optionsDecoder
        |> Resolve.optional "value" Resolve.string Nothing
        |> Resolve.optional "placeholder" Resolve.string Nothing
        |> Resolve.optional "label" Resolve.string Nothing


bindingsDecoder : Dict String (Value -> EventHandle msg) -> DropdownBindings msg
bindingsDecoder =
    Bind.succeed DropdownBindings |> Bind.bindableTyped "value" Json.Encode.string


component : Component msg
component =
    registerStateful
        { init = \_ -> initLocal
        , update = updateLocal
        , propsDecoder = propsDecoder
        , bindingsDecoder = bindingsDecoder
        , validationDecoder = \_ -> ()
        , view = viewDropdown
        }


{-| View function for the Dropdown. Used by the generated Browser.element wrapper.
-}
viewDropdown : LocalModel -> (LocalMsg -> msg) -> ComponentContext DropdownProps (DropdownBindings msg) () msg -> Html msg
viewDropdown local toMsg ctx =
    let
        selectedLabel =
            case ctx.props.value of
                Just val ->
                    List.filter (\opt -> opt.value == val) ctx.props.options
                        |> List.head
                        |> Maybe.map .label

                Nothing ->
                    Nothing
    in
    div [ class "jr-dropdown" ]
        [ -- Label
          case ctx.props.label of
            Just lbl ->
                Html.label [ class "jr-dropdown-label" ] [ text lbl ]

            Nothing ->
                text ""
        , -- Trigger button
          button
            [ class "jr-dropdown-trigger"
            , Html.Events.onClick (toMsg ToggleOpen)
            ]
            [ case selectedLabel of
                Just lbl ->
                    span [ class "jr-dropdown-trigger-value" ] [ text lbl ]

                Nothing ->
                    span [ class "jr-dropdown-trigger-placeholder" ]
                        [ text (Maybe.withDefault "Select..." ctx.props.placeholder) ]
            , span [ class "jr-dropdown-chevron" ] [ text "▾" ]
            ]
        , -- Panel (open state)
          if local.open then
            viewPanel local toMsg ctx

          else
            text ""
        ]


viewPanel : LocalModel -> (LocalMsg -> msg) -> ComponentContext DropdownProps (DropdownBindings msg) () msg -> Html msg
viewPanel local toMsg ctx =
    let
        filtered =
            if String.isEmpty local.searchText then
                ctx.props.options

            else
                let
                    search =
                        String.toLower local.searchText
                in
                List.filter
                    (\opt -> String.contains search (String.toLower opt.label))
                    ctx.props.options

        highlightedOpt =
            List.drop local.highlightedIndex filtered
                |> List.head
    in
    div [ class "jr-dropdown-panel" ]
        [ input
            [ type_ "text"
            , class "jr-dropdown-search"
            , placeholder "Search..."
            , value local.searchText
            , Html.Events.onInput (\s -> toMsg (SetSearch s))
            , Html.Events.preventDefaultOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.map
                        (\key ->
                            case key of
                                "ArrowDown" ->
                                    ( toMsg (HighlightIndex (min (local.highlightedIndex + 1) (List.length filtered - 1)))
                                    , True
                                    )

                                "ArrowUp" ->
                                    ( toMsg (HighlightIndex (max (local.highlightedIndex - 1) 0))
                                    , True
                                    )

                                "Enter" ->
                                    case highlightedOpt of
                                        Just opt ->
                                            ( toMsg (SelectOption opt), True )

                                        Nothing ->
                                            ( toMsg CloseDropdown, True )

                                "Escape" ->
                                    ( toMsg CloseDropdown, True )

                                _ ->
                                    ( toMsg (KeyDown key), False )
                        )
                )
            ]
            []
        , div [ class "jr-dropdown-options" ]
            (List.indexedMap
                (\i opt ->
                    button
                        [ class
                            ("jr-dropdown-option"
                                ++ (if i == local.highlightedIndex then
                                        " jr-dropdown-option-highlighted"

                                    else
                                        ""
                                   )
                            )
                        , Html.Events.onClick (toMsg (SelectOption opt))
                        , Html.Events.onMouseEnter (toMsg (HighlightIndex i))
                        ]
                        [ text opt.label ]
                )
                filtered
            )
        ]
