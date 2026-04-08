port module Catalog.MiniApps.Dropdown exposing (main)

import Browser
import Catalog.Components.Dropdown as Dropdown exposing (DropdownProps, LocalModel, LocalMsg(..))
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Resolve as Resolve


port propsIn : (Value -> msg) -> Sub msg


port actionsOut : Value -> Cmd msg


type alias Model =
    { props : Result String DropdownProps
    , local : LocalModel
    , bindingPath : Maybe String
    }


type Msg
    = PropsUpdated Value
    | Local LocalMsg


decodeProps : Value -> Result String DropdownProps
decodeProps flags =
    case Decode.decodeValue (Decode.field "props" Decode.value) flags of
        Ok propsValue ->
            case Decode.decodeValue (Decode.dict Resolve.valueDecoder) propsValue of
                Ok resolvedDict ->
                    Dropdown.propsDecoder resolvedDict

                Err err ->
                    Err (Decode.errorToString err)

        Err err ->
            Err (Decode.errorToString err)


decodeBindingPath : Value -> Maybe String
decodeBindingPath flags =
    Decode.decodeValue
        (Decode.at [ "bindings", "value" ] Decode.string)
        flags
        |> Result.toMaybe


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { props = decodeProps flags
      , local = Dropdown.initLocal
      , bindingPath = decodeBindingPath flags
      }
    , actionsOut (Encode.object [])
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PropsUpdated flags ->
            ( { model
                | props = decodeProps flags
                , bindingPath = decodeBindingPath flags
              }
            , Cmd.none
            )

        Local localMsg ->
            let
                newLocal =
                    Dropdown.updateLocal localMsg model.local
            in
            case localMsg of
                SelectOption opt ->
                    ( { model | local = newLocal }
                    , case model.bindingPath of
                        Just path ->
                            actionsOut
                                (Encode.object
                                    [ ( "type", Encode.string "binding" )
                                    , ( "path", Encode.string path )
                                    , ( "value", Encode.string opt.value )
                                    ]
                                )

                        Nothing ->
                            Cmd.none
                    )

                _ ->
                    ( { model | local = newLocal }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.props of
        Ok props ->
            viewDropdown props model.local

        Err err ->
            Html.div [] [ Html.text ("Props error: " ++ err) ]


viewDropdown : DropdownProps -> LocalModel -> Html Msg
viewDropdown props local =
    let
        selectedLabel =
            case props.value of
                Just val ->
                    List.filter (\opt -> opt.value == val) props.options
                        |> List.head
                        |> Maybe.map .label

                Nothing ->
                    Nothing
    in
    div [ class "jr-dropdown" ]
        [ case props.label of
            Just lbl ->
                Html.label [ class "jr-dropdown-label" ] [ text lbl ]

            Nothing ->
                text ""
        , button
            [ class "jr-dropdown-trigger"
            , Html.Events.onClick (Local ToggleOpen)
            ]
            [ case selectedLabel of
                Just lbl ->
                    span [ class "jr-dropdown-trigger-value" ] [ text lbl ]

                Nothing ->
                    span [ class "jr-dropdown-trigger-placeholder" ]
                        [ text (Maybe.withDefault "Select..." props.placeholder) ]
            , span [ class "jr-dropdown-chevron" ] [ text "▾" ]
            ]
        , if local.open then
            viewPanel props local

          else
            text ""
        ]


viewPanel : DropdownProps -> LocalModel -> Html Msg
viewPanel props local =
    let
        filtered =
            if String.isEmpty local.searchText then
                props.options

            else
                let
                    search =
                        String.toLower local.searchText
                in
                List.filter
                    (\opt -> String.contains search (String.toLower opt.label))
                    props.options

        highlightedOpt =
            List.drop local.highlightedIndex filtered |> List.head
    in
    div [ class "jr-dropdown-panel" ]
        [ input
            [ type_ "text"
            , class "jr-dropdown-search"
            , placeholder "Search..."
            , value local.searchText
            , Html.Events.onInput (\s -> Local (SetSearch s))
            , Html.Events.preventDefaultOn "keydown"
                (Decode.field "key" Decode.string
                    |> Decode.map
                        (\key ->
                            case key of
                                "ArrowDown" ->
                                    ( Local (HighlightIndex (min (local.highlightedIndex + 1) (List.length filtered - 1)))
                                    , True
                                    )

                                "ArrowUp" ->
                                    ( Local (HighlightIndex (max (local.highlightedIndex - 1) 0))
                                    , True
                                    )

                                "Enter" ->
                                    case highlightedOpt of
                                        Just opt ->
                                            ( Local (SelectOption opt), True )

                                        Nothing ->
                                            ( Local CloseDropdown, True )

                                "Escape" ->
                                    ( Local CloseDropdown, True )

                                _ ->
                                    ( Local (KeyDown key), False )
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
                        , Html.Events.onClick (Local (SelectOption opt))
                        , Html.Events.onMouseEnter (Local (HighlightIndex i))
                        ]
                        [ text opt.label ]
                )
                filtered
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    propsIn PropsUpdated


main : Program Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
