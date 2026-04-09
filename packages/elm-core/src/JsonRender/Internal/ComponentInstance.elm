module JsonRender.Internal.ComponentInstance exposing (ComponentInstance(..), RawComponentContext)

{-| Internal module: ComponentInstance type and RawComponentContext alias.

Defined in an Internal module to avoid circular imports between Render.elm
(which creates instances) and Actions.elm (which stores them in Model).

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Encode exposing (Value)
import JsonRender.Internal.EventHandle exposing (EventHandle)
import JsonRender.Resolve exposing (ResolvedValue)
import JsonRender.Validation


{-| Raw (untyped) context passed to component view functions.
-}
type alias RawComponentContext msg =
    { props : Dict String ResolvedValue
    , bindings : Dict String (Value -> EventHandle msg)
    , validation : Dict String JsonRender.Validation.FieldValidation
    , children : List (Html msg)
    , emit : String -> EventHandle msg
    , validate : EventHandle msg
    , validateAndEmit : String -> EventHandle msg
    , validateOn : JsonRender.Validation.ValidateOn
    }


{-| A live component instance whose typed state is captured in closures.
-}
type ComponentInstance msg
    = ComponentInstance
        { view : RawComponentContext msg -> Html msg
        , onPropsChanged : RawComponentContext msg -> ( ComponentInstance msg, List (EventHandle msg) )
        }
