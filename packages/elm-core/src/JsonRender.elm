module JsonRender exposing
    ( App
    , Config
    , Model
    , create
    , decodePortIn
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
import JsonRender.Internal.Effect as Effect
import JsonRender.Internal.EventHandle exposing (EventHandle)
import JsonRender.Internal.PortCmd as PortCmd
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
    , componentPortOut : Maybe (Value -> Cmd msg)
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
    , pendingPortCmds = []
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
                    , pendingPortCmds = []
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

        drainPortCmds : List ( String, PortCmd.PortCmd ) -> Cmd msg
        drainPortCmds cmds =
            case config.componentPortOut of
                Just send ->
                    cmds
                        |> List.map (\( instanceId, cmd ) -> send (PortCmd.encode instanceId cmd))
                        |> Cmd.batch

                Nothing ->
                    Cmd.none
    in
    { update =
        \msg model ->
            let
                ( newJR, cmd ) =
                    Actions.update functions config.actionConfig msg (config.getModel model)

                portOutCmd =
                    drainPortCmds newJR.pendingPortCmds

                cleanJR =
                    { newJR | pendingPortCmds = [] }
            in
            ( config.setModel cleanJR model, Cmd.batch [ Cmd.map config.toMsg cmd, portOutCmd ] )
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
       , update : localMsg -> state -> ComponentContext props bindings validation (Msg action) -> ( state, List (Effect.Effect (Msg action) localMsg) )
       , view : state -> props -> (localMsg -> Msg action) -> List (Html (Msg action)) -> Html (Msg action)
       , onPropsChange : Maybe (props -> state -> ( state, List (Effect.Effect (Msg action) localMsg) ))
       , portSubscriptions : List ( String, Value -> localMsg )
       }
    -> Component (Msg action)
registerStateful =
    Render.registerStateful


{-| Decode a componentPortIn message and route it to the correct component instance.
-}
decodePortIn : (Msg action -> msg) -> Value -> msg
decodePortIn toMsg val =
    case Decode.decodeValue portInDecoder val of
        Ok ( instanceId, portName, value ) ->
            toMsg (Actions.PortIn instanceId portName value)

        Err _ ->
            toMsg (Actions.ActionError "Failed to decode componentPortIn message")


portInDecoder : Decode.Decoder ( String, String, Value )
portInDecoder =
    Decode.map3 (\a b c -> ( a, b, c ))
        (Decode.field "instanceId" Decode.string)
        (Decode.field "port" Decode.string)
        (Decode.field "value" Decode.value)


{-| Decoder for json-render specs.
-}
specDecoder : Decoder Spec
specDecoder =
    Spec.decoder
