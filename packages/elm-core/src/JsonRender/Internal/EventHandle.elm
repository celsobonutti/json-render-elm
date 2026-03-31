module JsonRender.Internal.EventHandle exposing (EventHandle(..), fromMsg, withPreventDefault)

{-| Internal module: opaque event handle carrying a message and preventDefault flag.

Components receive EventHandle values from emit and bindings but cannot construct
them directly — they must go through JsonRender.Events to attach event handlers.

-}


type EventHandle msg
    = EventHandle { message : msg, preventDefault : Bool }


{-| Wrap a message with a specific preventDefault flag.
Used by Render.buildEmit to carry the flag from the spec.
-}
withPreventDefault : Bool -> msg -> EventHandle msg
withPreventDefault pd msg =
    EventHandle { message = msg, preventDefault = pd }


{-| Wrap a message with preventDefault = False.
Used by extractBindings for binding setters.
-}
fromMsg : msg -> EventHandle msg
fromMsg msg =
    EventHandle { message = msg, preventDefault = False }
