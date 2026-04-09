module JsonRender exposing
    ( App
    , Config
    , Model
    , create
    , init
    , receiveSpec
    , register
    , registerStateful
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
import JsonRender.Internal.ComponentInstance exposing (ComponentInstance)
import JsonRender.Internal.EventHandle exposing (EventHandle)
import JsonRender.Render as Render exposing (Component, ComponentContext, Registry)
import JsonRender.Resolve exposing (ResolvedValue)
import JsonRender.Spec as Spec exposing (Spec)
import JsonRender.Validation as Validation
import Random


{-| The json-render model. Store this in your application model.
-}
type alias Model action =
    Actions.Model action


{-| Configuration for `create`.
-}
type alias Config action model msg =
    { actionConfig : Actions.ActionConfig action
    , registry : Registry (Msg action)
    , toMsg : Msg action -> msg
    , getModel : model -> Model action
    , setModel : Model action -> model -> model
    }


{-| The wired-up update and render functions returned by `create`.
-}
type alias App action model msg =
    { update : Msg action -> model -> ( model, Cmd msg )
    , render : model -> Html msg
    }


{-| Create an empty json-render model.
-}
init : Random.Seed -> Model action
init seed =
    { spec = Nothing
    , state = Encode.object []
    , seed = seed
    , validationState = Dict.empty
    , validationRegistry = Dict.empty
    , localComponents = Dict.empty
    }


{-| Decode and apply an incoming spec to the model.
-}
receiveSpec : Value -> Model action -> Result String (Model action)
receiveSpec val model =
    case Decode.decodeValue Spec.decoder val of
        Ok spec ->
            Ok
                { model
                    | spec = Just spec
                    , state = Maybe.withDefault model.state spec.state
                    , localComponents = Dict.empty
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
                    Html.map config.toMsg (Render.render config.registry jr.state jr.validationState jr.localComponents spec)

                Nothing ->
                    Html.text ""
    }


{-| Render a spec to Html using the given registry, state, validation state, and local components.
-}
render : Registry (Msg action) -> Value -> Dict String Validation.FieldValidation -> Dict String (ComponentInstance (Msg action)) -> Spec -> Html (Msg action)
render =
    Render.render


{-| Register a component with a props decoder, bindings decoder, validation decoder, and view function.
-}
register :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> EventHandle (Msg action)) -> bindings)
    -> (Dict String Validation.FieldValidation -> validation)
    -> (ComponentContext props bindings validation (Msg action) -> Html (Msg action))
    -> Component (Msg action)
register =
    Render.register


{-| Register a stateful component with local state lifecycle.
-}
registerStateful :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> EventHandle (Msg action)) -> bindings)
    -> (Dict String Validation.FieldValidation -> validation)
    -> { init : props -> state
       , update : localMsg -> state -> ComponentContext props bindings validation (Msg action) -> ( state, List (EventHandle (Msg action)) )
       , view : state -> props -> (localMsg -> Msg action) -> List (Html (Msg action)) -> Html (Msg action)
       , onPropsChange : Maybe (props -> state -> ( state, List (EventHandle (Msg action)) ))
       }
    -> Component (Msg action)
registerStateful =
    Render.registerStateful


{-| Decoder for json-render specs.
-}
specDecoder : Decoder Spec
specDecoder =
    Spec.decoder
