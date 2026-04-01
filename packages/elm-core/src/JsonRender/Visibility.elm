module JsonRender.Visibility exposing
    ( decoder
    , evaluate
    )

{-| Conditional visibility for spec elements.

Thin wrapper re-exporting from Internal.Condition. Types are accessed
directly from JsonRender.Internal.Condition.

-}

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import JsonRender.Internal.Condition as Condition exposing (Condition, RepeatContext)


decoder : Decoder Condition
decoder =
    Condition.decoder


evaluate : Value -> Maybe RepeatContext -> Condition -> Result String Bool
evaluate =
    Condition.evaluate
