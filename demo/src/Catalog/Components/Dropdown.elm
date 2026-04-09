module Catalog.Components.Dropdown exposing (DropdownBindings, DropdownProps, bindingsDecoder, component, propsDecoder)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Bind as Bind
import JsonRender.Events exposing (EventHandle)
import JsonRender.Internal.EventHandle as EventHandle
import JsonRender.Render exposing (Component, ComponentContext, register)
import JsonRender.Resolve as ResolvedValue exposing (ResolvedValue)


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


propsDecoder : Dict String ResolvedValue -> Result String DropdownProps
propsDecoder =
    ResolvedValue.succeed DropdownProps
        |> ResolvedValue.required "options" decodeOptions
        |> ResolvedValue.optional "value" ResolvedValue.string Nothing
        |> ResolvedValue.optional "placeholder" ResolvedValue.string Nothing
        |> ResolvedValue.optional "label" ResolvedValue.string Nothing


decodeOptions : ResolvedValue -> Result String (List DropdownOption)
decodeOptions rv =
    case rv of
        ResolvedValue.RList items ->
            items
                |> List.map decodeOption
                |> combineResults

        _ ->
            Err "options must be a list"


decodeOption : ResolvedValue -> Result String DropdownOption
decodeOption rv =
    ResolvedValue.object rv
        |> Result.andThen
            (\dict ->
                Result.map2 DropdownOption
                    (Dict.get "label" dict
                        |> Maybe.map ResolvedValue.string
                        |> Maybe.withDefault (Err "missing label")
                    )
                    (Dict.get "value" dict
                        |> Maybe.map ResolvedValue.string
                        |> Maybe.withDefault (Err "missing value")
                    )
            )


combineResults : List (Result String a) -> Result String (List a)
combineResults results =
    List.foldr
        (\r acc ->
            Result.map2 (::) r acc
        )
        (Ok [])
        results


bindingsDecoder : Dict String (Value -> EventHandle msg) -> DropdownBindings msg
bindingsDecoder =
    Bind.succeed DropdownBindings |> Bind.bindableTyped "value" Encode.string


component : Component msg
component =
    register propsDecoder bindingsDecoder (\_ -> ()) view


view : ComponentContext DropdownProps (DropdownBindings msg) () msg -> Html msg
view ctx =
    Html.node "jr-dropdown"
        ([ Html.Attributes.attribute "options" (Encode.encode 0 (encodeOptions ctx.props.options))
         , Html.Attributes.attribute "value" (Maybe.withDefault "" ctx.props.value)
         , Html.Attributes.attribute "placeholder" (Maybe.withDefault "Select..." ctx.props.placeholder)
         , Html.Events.on "jr-action" (decodeJrAction ctx.bindings)
         ]
            ++ (case ctx.props.label of
                    Just lbl ->
                        [ Html.Attributes.attribute "label" lbl ]

                    Nothing ->
                        []
               )
        )
        []


encodeOptions : List DropdownOption -> Value
encodeOptions options =
    Encode.list
        (\opt ->
            Encode.object
                [ ( "label", Encode.string opt.label )
                , ( "value", Encode.string opt.value )
                ]
        )
        options


decodeJrAction : DropdownBindings msg -> Decode.Decoder msg
decodeJrAction bindings =
    Decode.at [ "detail", "value" ] Decode.string
        |> Decode.andThen
            (\val ->
                case bindings.value of
                    Just setValue ->
                        let
                            (EventHandle.EventHandle { message }) =
                                setValue val
                        in
                        Decode.succeed message

                    Nothing ->
                        Decode.fail "no value binding"
            )
