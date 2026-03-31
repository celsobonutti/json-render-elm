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


type alias Model =
    Actions.Model


type alias Config action model msg =
    { actionConfig : Actions.ActionConfig action
    , registry : Registry (Msg action)
    , toMsg : Msg action -> msg
    , getModel : model -> Model
    , setModel : Model -> model -> model
    }


type alias App action model msg =
    { update : Msg action -> model -> ( model, Cmd msg )
    , render : model -> Html msg
    }


init : Random.Seed -> Model
init seed =
    { spec = Nothing
    , state = Encode.object []
    , seed = seed
    }


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


render : Registry (Msg action) -> Value -> Spec -> Html (Msg action)
render =
    Render.render


register :
    (Dict String ResolvedValue -> Result String props)
    -> (Dict String (Value -> Msg action) -> bindings)
    -> (ComponentContext props bindings (Msg action) -> Html (Msg action))
    -> Component (Msg action)
register =
    Render.register


specDecoder : Decoder Spec
specDecoder =
    Spec.decoder
