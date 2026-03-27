module JsonRender.Spec exposing
    ( ActionBinding
    , Element
    , EventHandler(..)
    , Repeat
    , Spec
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
import Json.Encode as Encode exposing (Value)
import JsonRender.Internal.PropValue as PropValue exposing (PropValue)
import JsonRender.Visibility exposing (VisibilityCondition)


type alias Spec =
    { root : String
    , elements : Dict String Element
    , state : Maybe Value
    }


type alias Element =
    { type_ : String
    , props : Dict String PropValue
    , children : List String
    , visible : Maybe VisibilityCondition
    , repeat : Maybe Repeat
    , on : Dict String EventHandler
    , watch : Dict String EventHandler
    }


type alias Repeat =
    { statePath : String
    , key : Maybe String
    }


type alias ActionBinding =
    { action : String
    , params : Dict String PropValue
    }


type EventHandler
    = SingleAction ActionBinding
    | ChainedActions (List ActionBinding)


decoder : Decoder Spec
decoder =
    Decode.succeed Spec
        |> required "root" Decode.string
        |> required "elements" (Decode.dict elementDecoder)
        |> optional "state" (Decode.map Just Decode.value) Nothing


elementDecoder : Decoder Element
elementDecoder =
    Decode.succeed Element
        |> required "type" Decode.string
        |> required "props" (Decode.dict PropValue.decoder)
        |> optional "children" (Decode.list Decode.string) []
        |> optional "visible" (Decode.map Just JsonRender.Visibility.decoder) Nothing
        |> optional "repeat" (Decode.map Just repeatDecoder) Nothing
        |> optional "on" (Decode.dict eventHandlerDecoder) Dict.empty
        |> optional "watch" (Decode.dict eventHandlerDecoder) Dict.empty


repeatDecoder : Decoder Repeat
repeatDecoder =
    Decode.succeed Repeat
        |> required "statePath" Decode.string
        |> optional "key" (Decode.map Just Decode.string) Nothing


actionBindingDecoder : Decoder ActionBinding
actionBindingDecoder =
    Decode.succeed ActionBinding
        |> required "action" Decode.string
        |> optional "params" (Decode.dict PropValue.decoder) Dict.empty


eventHandlerDecoder : Decoder EventHandler
eventHandlerDecoder =
    Decode.oneOf
        [ Decode.list actionBindingDecoder |> Decode.map ChainedActions
        , actionBindingDecoder |> Decode.map SingleAction
        ]


{-| Re-export for convenience.
-}
propValueDecoder : Decoder PropValue
propValueDecoder =
    PropValue.decoder
