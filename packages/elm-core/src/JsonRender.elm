module JsonRender exposing
    ( render
    , register
    , specDecoder
    )

{-| Convenience re-exports for json-render-elm.

For types, import the specific modules:

  - `JsonRender.Spec` for `Spec`, `Element`
  - `JsonRender.Internal.PropValue` for `PropValue(..)`
  - `JsonRender.Resolve` for `ResolvedValue(..)`
  - `JsonRender.Render` for `Component`, `Registry`, `ComponentContext`
  - `JsonRender.Actions` for `Msg(..)`, `Model`

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import JsonRender.Actions exposing (Msg)
import JsonRender.Render as Render exposing (Component, ComponentContext, Registry)
import JsonRender.Resolve exposing (ResolvedValue)
import JsonRender.Spec as Spec exposing (Spec)


{-| Render a spec to Html using the given registry and state.
-}
render : Registry -> Value -> Spec -> Html Msg
render =
    Render.render


{-| Register a component with a props decoder, bindings decoder, and view function.
-}
register :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> Msg) -> bindings)
    -> (ComponentContext props bindings -> Html Msg)
    -> Component
register =
    Render.register


{-| Decoder for json-render specs.
-}
specDecoder : Decoder Spec
specDecoder =
    Spec.decoder
