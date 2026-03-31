module JsonRender.Events exposing
    ( EventHandle
    , on
    , onBlur
    , onCheck
    , onClick
    , onDoubleClick
    , onFocus
    , onInput
    , onMouseDown
    , onMouseEnter
    , onMouseLeave
    , onMouseUp
    , onSubmit
    )

{-| Event handlers for json-render components.

Mirrors Html.Events but consumes EventHandle values, automatically applying
preventDefault when the spec's action binding requests it. Components MUST use
this module instead of Html.Events — the types enforce this since emit and
bindings return EventHandle msg, not raw msg.

-}

import Html exposing (Attribute)
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import JsonRender.Internal.EventHandle as EventHandle exposing (EventHandle(..))


{-| Re-export for component authors — they don't need to import Internal.EventHandle.
-}
type alias EventHandle msg =
    EventHandle.EventHandle msg


onClick : EventHandle msg -> Attribute msg
onClick (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "click"
        (Decode.succeed ( message, preventDefault ))


onDoubleClick : EventHandle msg -> Attribute msg
onDoubleClick (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "dblclick"
        (Decode.succeed ( message, preventDefault ))


onMouseDown : EventHandle msg -> Attribute msg
onMouseDown (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "mousedown"
        (Decode.succeed ( message, preventDefault ))


onMouseUp : EventHandle msg -> Attribute msg
onMouseUp (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "mouseup"
        (Decode.succeed ( message, preventDefault ))


onMouseEnter : EventHandle msg -> Attribute msg
onMouseEnter (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "mouseenter"
        (Decode.succeed ( message, preventDefault ))


onMouseLeave : EventHandle msg -> Attribute msg
onMouseLeave (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "mouseleave"
        (Decode.succeed ( message, preventDefault ))


onBlur : EventHandle msg -> Attribute msg
onBlur (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "blur"
        (Decode.succeed ( message, preventDefault ))


onFocus : EventHandle msg -> Attribute msg
onFocus (EventHandle { message, preventDefault }) =
    Html.Events.preventDefaultOn "focus"
        (Decode.succeed ( message, preventDefault ))


{-| Matches Elm's Html.Events.onInput: always stops propagation.
-}
onInput : (String -> EventHandle msg) -> Attribute msg
onInput toHandle =
    Html.Events.custom "input"
        (Html.Events.targetValue
            |> Decode.map
                (\str ->
                    let
                        (EventHandle { message, preventDefault }) =
                            toHandle str
                    in
                    { message = message
                    , stopPropagation = True
                    , preventDefault = preventDefault
                    }
                )
        )


onCheck : (Bool -> EventHandle msg) -> Attribute msg
onCheck toHandle =
    Html.Events.custom "change"
        (Html.Events.targetChecked
            |> Decode.map
                (\checked ->
                    let
                        (EventHandle { message, preventDefault }) =
                            toHandle checked
                    in
                    { message = message
                    , stopPropagation = False
                    , preventDefault = preventDefault
                    }
                )
        )


{-| Always prevents default, matching Elm's Html.Events.onSubmit.
Use `on "submit"` if you need to respect the flag instead.
-}
onSubmit : EventHandle msg -> Attribute msg
onSubmit (EventHandle { message }) =
    Html.Events.preventDefaultOn "submit"
        (Decode.succeed ( message, True ))


{-| Generic event handler. Respects the preventDefault flag from the EventHandle.
-}
on : String -> Decoder (EventHandle msg) -> Attribute msg
on event decoder =
    Html.Events.custom event
        (decoder
            |> Decode.map
                (\(EventHandle { message, preventDefault }) ->
                    { message = message
                    , stopPropagation = False
                    , preventDefault = preventDefault
                    }
                )
        )
