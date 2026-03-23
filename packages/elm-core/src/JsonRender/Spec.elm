module JsonRender.Spec exposing
    ( Spec
    , Element
    , decoder
    , propValueDecoder
    )

{-| Core types and JSON decoder for json-render specs.

Re-exports PropValue from Internal.PropValue to provide a public API
while avoiding circular dependencies with Visibility.
-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import JsonRender.Internal.PropValue as PropValue exposing (PropValue(..))
import JsonRender.Visibility exposing (VisibilityCondition)


type alias Spec =
    { root : String
    , elements : Dict String Element
    }


type alias Element =
    { type_ : String
    , props : Dict String PropValue
    , children : List String
    , visible : Maybe VisibilityCondition
    }


decoder : Decoder Spec
decoder =
    Decode.succeed Spec
        |> required "root" Decode.string
        |> required "elements" (Decode.dict elementDecoder)


elementDecoder : Decoder Element
elementDecoder =
    Decode.succeed Element
        |> required "type" Decode.string
        |> required "props" (Decode.dict PropValue.decoder)
        |> required "children" (Decode.list Decode.string)
        |> optional "visible" (Decode.map Just JsonRender.Visibility.decoder) Nothing


{-| Re-export for convenience.
-}
propValueDecoder : Decoder PropValue
propValueDecoder =
    PropValue.decoder
