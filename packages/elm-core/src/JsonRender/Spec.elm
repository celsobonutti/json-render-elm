module JsonRender.Spec exposing
    ( ActionBinding
    , Element
    , EventHandler(..)
    , Repeat
    , Spec
    , decoder
    , propValueDecoder
    , shouldPreventDefault
    )

{-| Core types and JSON decoder for json-render specs.

Re-exports PropValue from Internal.PropValue to provide a public API
while avoiding circular dependencies with Visibility.

-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode exposing (Value)
import JsonRender.Internal.PropValue as PropValue exposing (PropValue)
import JsonRender.Internal.Condition exposing (Condition)
import JsonRender.Validation as Validation exposing (ValidateOn(..), ValidationCheck)
import JsonRender.Visibility


type alias Spec =
    { root : String
    , elements : Dict String Element
    , state : Maybe Value
    }


type alias Element =
    { type_ : String
    , props : Dict String PropValue
    , children : List String
    , visible : Maybe Condition
    , repeat : Maybe Repeat
    , on : Dict String EventHandler
    , watch : Dict String EventHandler
    , enabled : Maybe Condition
    , checks : List ValidationCheck
    , validateOn : ValidateOn
    }


type alias Repeat =
    { statePath : String
    , key : Maybe String
    }


type alias ActionBinding =
    { action : String
    , params : Dict String PropValue
    , preventDefault : Bool
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
        |> optional "enabled" (Decode.map Just JsonRender.Visibility.decoder) Nothing
        |> optional "checks" (Decode.list Validation.checkDecoder) []
        |> optional "validateOn" Validation.validateOnDecoder OnSubmit


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
        |> optional "preventDefault" Decode.bool False


eventHandlerDecoder : Decoder EventHandler
eventHandlerDecoder =
    Decode.oneOf
        [ Decode.list actionBindingDecoder |> Decode.map ChainedActions
        , actionBindingDecoder |> Decode.map SingleAction
        ]


{-| Returns True if any action binding in the handler has preventDefault set.
Matches React's `actionBindings.some(b => b.preventDefault)` semantics.
-}
shouldPreventDefault : EventHandler -> Bool
shouldPreventDefault handler =
    case handler of
        SingleAction binding ->
            binding.preventDefault

        ChainedActions bindings ->
            List.any .preventDefault bindings


{-| Re-export for convenience.
-}
propValueDecoder : Decoder PropValue
propValueDecoder =
    PropValue.decoder
