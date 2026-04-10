module JsonRender.Internal.Effect exposing (Effect(..), EffectResult(..))

import Json.Encode exposing (Value)
import JsonRender.Internal.EventHandle exposing (EventHandle)
import JsonRender.Internal.PortCmd exposing (PortCmd)


{-| Side effects returned by a stateful component's `update` function.

  - `Emit` — fire an event handle (binding update, emit, validate)
  - `SendPort` — send a value to JS via the component port system
  - `RunCmd` — run an Elm Cmd whose result routes back to this component's update

-}
type Effect msg localMsg
    = Emit (EventHandle msg)
    | SendPort String Value
    | RunCmd (Cmd localMsg)


{-| Processed effects with localMsg resolved to msg.
Used internally by the framework after routing RunCmd through instance dispatch.
-}
type EffectResult msg
    = EmitResult (EventHandle msg)
    | SendPortResult PortCmd
    | RunCmdResult (Cmd msg)
