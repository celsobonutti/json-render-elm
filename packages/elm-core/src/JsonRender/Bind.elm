module JsonRender.Bind exposing (BindingsDecoder, succeed, bindable, bindableTyped)

{-| Pipeline-style combinators for typed bindings decoders.

Works like JsonRender.Resolve's pipeline decoders but for binding
setter functions instead of prop values.

-}

import Dict exposing (Dict)
import Json.Encode exposing (Value)


{-| A function that decodes a raw bindings dict into a typed bindings record.
-}
type alias BindingsDecoder msg a =
    Dict String (Value -> msg) -> a


{-| Start a bindings decoder pipeline.
-}
succeed : a -> BindingsDecoder msg a
succeed a _ =
    a


{-| Extract a binding setter for the given prop name.

Returns `Just setter` if the prop had a `$bindState` or `$bindItem` expression,
`Nothing` otherwise.

-}
bindable : String -> BindingsDecoder msg (Maybe (Value -> msg) -> b) -> BindingsDecoder msg b
bindable key prev dict =
    prev dict (Dict.get key dict)


{-| Like `bindable`, but wraps the setter with an encoder so the caller
works with a typed value instead of raw `Value`.

    bindableTyped "age" Json.Encode.int

yields `Maybe (Int -> msg)` instead of `Maybe (Value -> msg)`.

-}
bindableTyped : String -> (a -> Value) -> BindingsDecoder msg (Maybe (a -> msg) -> b) -> BindingsDecoder msg b
bindableTyped key encode prev dict =
    prev dict (Dict.get key dict |> Maybe.map (\setter -> setter << encode))
