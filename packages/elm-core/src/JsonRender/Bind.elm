module JsonRender.Bind exposing (BindingsDecoder, succeed, bindable)

{-| Pipeline-style combinators for typed bindings decoders.

Works like JsonRender.Resolve's pipeline decoders but for binding
setter functions instead of prop values.

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)
import JsonRender.Actions exposing (Msg)


{-| A function that decodes a raw bindings dict into a typed bindings record.
-}
type alias BindingsDecoder action a =
    Dict String (Value -> Msg action) -> a


{-| Start a bindings decoder pipeline.
-}
succeed : a -> BindingsDecoder action a
succeed a _ =
    a


{-| Extract a binding setter for the given prop name.

Returns `Just setter` if the prop had a `$bindState` or `$bindItem` expression,
`Nothing` otherwise.

-}
bindable : String -> BindingsDecoder action (Maybe (Value -> Msg action) -> b) -> BindingsDecoder action b
bindable key prev dict =
    prev dict (Dict.get key dict)
