module JsonRender exposing
    ( App
    , Config
    , Model
    , create
    , init
    , receiveSpec
    , register
    , render
    , specDecoder
    )

{-| Entry point for json-render-elm.

Use `create` to wire a `Registry` and `ActionConfig` into ready-to-use
`update` and `render` functions with zero boilerplate.

For types, import the specific modules:

  - `JsonRender.Spec` for `Spec`, `Element`
  - `JsonRender.Internal.PropValue` for `PropValue(..)`
  - `JsonRender.Resolve` for `ResolvedValue(..)`
  - `JsonRender.Render` for `Component`, `Registry`, `ComponentContext`
  - `JsonRender.Actions` for `Msg(..)`, `ActionConfig`
  - `JsonRender.Bind` for binding combinators

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import JsonRender.Actions as Actions exposing (Msg)
import JsonRender.Render as Render exposing (Component, ComponentContext, Registry)
import JsonRender.Resolve exposing (ResolvedValue)
import JsonRender.Spec as Spec exposing (Spec)
import Random


{-| The json-render model. Store this in your application model.
-}
type alias Model =
    Actions.Model


{-| Configuration for `create`.
-}
type alias Config action model msg =
    { actionConfig : Actions.ActionConfig action
    , registry : Registry (Msg action)
    , toMsg : Msg action -> msg
    , getModel : model -> Model
    , setModel : Model -> model -> model
    }


{-| The wired-up update and render functions returned by `create`.
-}
type alias App action model msg =
    { update : Msg action -> model -> ( model, Cmd msg )
    , render : model -> Html msg
    }


{-| Create an empty json-render model.
-}
init : Random.Seed -> Model
init seed =
    { spec = Nothing
    , state = Encode.object []
    , seed = seed
    }


{-| Decode and apply an incoming spec to the model.
-}
receiveSpec : Value -> Model -> Result String Model
receiveSpec val model =
    case Decode.decodeValue Spec.decoder val of
        Ok spec ->
            Ok
                { model
                    | spec = Just spec
                    , state = Maybe.withDefault model.state spec.state
                }

        Err err ->
            Err (Decode.errorToString err)


{-| Wire a Registry and ActionConfig into ready-to-use update and render functions.
-}
create : Config action model msg -> App action model msg
create config =
    let
        functions =
            config.registry.functions
    in
    { update =
        \msg model ->
            let
                ( newJR, cmd ) =
                    Actions.update functions config.actionConfig msg (config.getModel model)
            in
            ( config.setModel newJR model, Cmd.map config.toMsg cmd )
    , render =
        \model ->
            let
                jr =
                    config.getModel model
            in
            case jr.spec of
                Just spec ->
                    Html.map config.toMsg (Render.render config.registry jr.state spec)

                Nothing ->
                    Html.text ""
    }


{-| Render a spec to Html using the given registry and state.
-}
render : Registry (Msg action) -> Value -> Spec -> Html (Msg action)
render =
    Render.render


{-| Register a component with a props decoder, bindings decoder, and view function.
-}
register :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> Msg action) -> bindings)
    -> (ComponentContext props bindings (Msg action) -> Html (Msg action))
    -> Component (Msg action)
register =
    Render.register


{-| Decoder for json-render specs.
-}
specDecoder : Decoder Spec
specDecoder =
    Spec.decoder
