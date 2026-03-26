module ActionsTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Actions as Actions exposing (Msg(..))
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Spec as Spec
import JsonRender.State as State
import Test exposing (..)


type TestAction
    = TestPress
    | TestExport { format : String }


testActionConfig : Actions.ActionConfig TestAction
testActionConfig =
    { handleAction = \_ model -> ( model, Cmd.none )
    , decodeAction = decodeTestAction
    , functions = Dict.empty
    }


decodeTestAction : String -> Dict.Dict String Encode.Value -> Result String TestAction
decodeTestAction name params =
    case name of
        "press" ->
            Ok TestPress

        "export" ->
            case Dict.get "format" params of
                Just fmt ->
                    case Decode.decodeValue Decode.string fmt of
                        Ok s ->
                            Ok (TestExport { format = s })

                        Err _ ->
                            Err "export: format must be a string"

                Nothing ->
                    Err "export: missing format param"

        _ ->
            Err ("Unknown action: " ++ name)


suite : Test
suite =
    describe "JsonRender.Actions"
        [ describe "built-in actions (legacy Msg variants)"
            [ test "SetState updates state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object [ ( "name", Encode.string "Alice" ) ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (SetState "/name" (Encode.string "Bob")) model
                    in
                    State.get "/name" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Bob")
            , test "PushState appends to array" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object [ ( "items", Encode.list Encode.string [ "a" ] ) ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (PushState "/items" (Encode.string "b")) model
                    in
                    State.get "/items/1" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "b")
            , test "RemoveState removes from state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state =
                                Encode.object
                                    [ ( "name", Encode.string "Alice" )
                                    , ( "age", Encode.int 30 )
                                    ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (RemoveState "/age") model
                    in
                    State.get "/age" newModel.state
                        |> Expect.equal Nothing
            , test "CustomAction delegates to handler" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object []
                            }

                        config =
                            { handleAction =
                                \action m ->
                                    case action of
                                        TestExport { format } ->
                                            ( { m | state = Encode.object [ ( "exported", Encode.string format ) ] }
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( m, Cmd.none )
                            , decodeAction = decodeTestAction
                            , functions = Dict.empty
                            }

                        ( newModel, _ ) =
                            Actions.update config (CustomAction (TestExport { format = "pdf" })) model
                    in
                    State.get "/exported" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "pdf")
            ]
        , describe "action execution (ExecuteAction)"
            [ test "ExecuteAction with setState resolves params and updates state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object [ ( "clicked", Encode.bool False ) ]
                            }

                        binding =
                            { action = "setState"
                            , params =
                                Dict.fromList
                                    [ ( "path", StringValue "/clicked" )
                                    , ( "value", BoolValue True )
                                    ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteAction binding Nothing) model
                    in
                    State.get "/clicked" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.bool >> Result.toMaybe)
                        |> Expect.equal (Just True)
            , test "ExecuteAction with pushState appends to array" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state =
                                Encode.object
                                    [ ( "items", Encode.list Encode.string [ "first" ] ) ]
                            }

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "path", StringValue "/items" )
                                    , ( "value", StringValue "second" )
                                    ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteAction binding Nothing) model
                    in
                    State.get "/items/1" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "second")
            , test "ExecuteAction with removeState removes from state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state =
                                Encode.object
                                    [ ( "name", Encode.string "Alice" )
                                    , ( "temp", Encode.string "remove me" )
                                    ]
                            }

                        binding =
                            { action = "removeState"
                            , params =
                                Dict.fromList
                                    [ ( "path", StringValue "/temp" ) ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteAction binding Nothing) model
                    in
                    Expect.all
                        [ \m -> State.get "/temp" m.state |> Expect.equal Nothing
                        , \m ->
                            State.get "/name" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "Alice")
                        ]
                        newModel
            , test "ExecuteAction with custom action name calls decodeAction" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object []
                            }

                        exportConfig =
                            { handleAction =
                                \action m ->
                                    case action of
                                        TestExport { format } ->
                                            ( { m | state = Encode.object [ ( "exported", Encode.string format ) ] }
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( m, Cmd.none )
                            , decodeAction = decodeTestAction
                            , functions = Dict.empty
                            }

                        binding =
                            { action = "export"
                            , params =
                                Dict.fromList
                                    [ ( "format", StringValue "csv" ) ]
                            }

                        ( newModel, _ ) =
                            Actions.update exportConfig (ExecuteAction binding Nothing) model
                    in
                    State.get "/exported" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "csv")
            , test "ExecuteAction with unknown action name does not change state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object [ ( "x", Encode.int 1 ) ]
                            }

                        binding =
                            { action = "nonexistentAction"
                            , params = Dict.empty
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteAction binding Nothing) model
                    in
                    State.get "/x" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                        |> Expect.equal (Just 1)
            , test "ExecuteAction with $state expression resolves against current state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state =
                                Encode.object
                                    [ ( "source", Encode.string "hello" )
                                    , ( "items", Encode.list Encode.string [] )
                                    ]
                            }

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "path", StringValue "/items" )
                                    , ( "value", StateExpr "/source" )
                                    ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteAction binding Nothing) model
                    in
                    State.get "/items/0" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "hello")
            ]
        , describe "chained action execution (ExecuteChain)"
            [ test "ExecuteChain executes two actions in sequence" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state =
                                Encode.object
                                    [ ( "loading", Encode.bool False )
                                    , ( "items", Encode.list Encode.string [] )
                                    ]
                            }

                        bindings =
                            [ { action = "setState"
                              , params =
                                    Dict.fromList
                                        [ ( "path", StringValue "/loading" )
                                        , ( "value", BoolValue True )
                                        ]
                              }
                            , { action = "pushState"
                              , params =
                                    Dict.fromList
                                        [ ( "path", StringValue "/items" )
                                        , ( "value", StringValue "new item" )
                                        ]
                              }
                            ]

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteChain bindings Nothing) model
                    in
                    Expect.all
                        [ \m ->
                            State.get "/loading" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.bool >> Result.toMaybe)
                                |> Expect.equal (Just True)
                        , \m ->
                            State.get "/items/0" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "new item")
                        ]
                        newModel
            , test "ExecuteChain second action sees first action mutations via $state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state =
                                Encode.object
                                    [ ( "input", Encode.string "Buy milk" )
                                    , ( "items", Encode.list Encode.string [] )
                                    ]
                            }

                        bindings =
                            [ -- First: push current input value to items
                              { action = "pushState"
                              , params =
                                    Dict.fromList
                                        [ ( "path", StringValue "/items" )
                                        , ( "value", StateExpr "/input" )
                                        ]
                              }
                            , -- Second: clear input
                              { action = "setState"
                              , params =
                                    Dict.fromList
                                        [ ( "path", StringValue "/input" )
                                        , ( "value", StringValue "" )
                                        ]
                              }
                            ]

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteChain bindings Nothing) model
                    in
                    Expect.all
                        [ \m ->
                            State.get "/items/0" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "Buy milk")
                        , \m ->
                            State.get "/input" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "")
                        ]
                        newModel
            , test "ExecuteChain with empty list does not change state" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object [ ( "x", Encode.int 42 ) ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ExecuteChain [] Nothing) model
                    in
                    State.get "/x" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                        |> Expect.equal (Just 42)
            ]
        , describe "ActionError"
            [ test "ActionError does not change model" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state = Encode.object [ ( "x", Encode.int 1 ) ]
                            }

                        ( newModel, _ ) =
                            Actions.update testActionConfig (ActionError "No handler for event: tap") model
                    in
                    State.get "/x" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                        |> Expect.equal (Just 1)
            ]
        , describe "checkWatchers"
            [ test "fires watcher action when watched path changes" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "ok" },
                                  "children": [],
                                  "watch": {
                                    "/source": {
                                      "action": "setState",
                                      "params": { "path": "/derived", "value": "changed" }
                                    }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            let
                                oldState =
                                    Encode.object [ ( "source", Encode.string "a" ), ( "derived", Encode.string "" ) ]

                                model =
                                    { spec = Just spec
                                    , state = Encode.object [ ( "source", Encode.string "b" ), ( "derived", Encode.string "" ) ]
                                    }

                                ( newModel, _ ) =
                                    Actions.checkWatchers testActionConfig oldState model
                            in
                            State.get "/derived" newModel.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "changed")

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "does not fire watcher when watched path is unchanged" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "ok" },
                                  "children": [],
                                  "watch": {
                                    "/source": {
                                      "action": "setState",
                                      "params": { "path": "/derived", "value": "changed" }
                                    }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            let
                                oldState =
                                    Encode.object [ ( "source", Encode.string "same" ), ( "derived", Encode.string "original" ) ]

                                model =
                                    { spec = Just spec
                                    , state = Encode.object [ ( "source", Encode.string "same" ), ( "derived", Encode.string "original" ) ]
                                    }

                                ( newModel, _ ) =
                                    Actions.checkWatchers testActionConfig oldState model
                            in
                            State.get "/derived" newModel.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "original")

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "fires chained watcher actions" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "ok" },
                                  "children": [],
                                  "watch": {
                                    "/trigger": [
                                      { "action": "setState", "params": { "path": "/a", "value": "one" } },
                                      { "action": "setState", "params": { "path": "/b", "value": "two" } }
                                    ]
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            let
                                oldState =
                                    Encode.object [ ( "trigger", Encode.bool False ) ]

                                model =
                                    { spec = Just spec
                                    , state = Encode.object [ ( "trigger", Encode.bool True ) ]
                                    }

                                ( newModel, _ ) =
                                    Actions.checkWatchers testActionConfig oldState model
                            in
                            Expect.all
                                [ \m ->
                                    State.get "/a" m.state
                                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                        |> Expect.equal (Just "one")
                                , \m ->
                                    State.get "/b" m.state
                                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                        |> Expect.equal (Just "two")
                                ]
                                newModel

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "cascading watchers fire in sequence" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "ok" },
                                  "children": [],
                                  "watch": {
                                    "/a": { "action": "setState", "params": { "path": "/b", "value": "from-a" } },
                                    "/b": { "action": "setState", "params": { "path": "/c", "value": "from-b" } }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            let
                                oldState =
                                    Encode.object
                                        [ ( "a", Encode.string "old" )
                                        , ( "b", Encode.string "" )
                                        , ( "c", Encode.string "" )
                                        ]

                                model =
                                    { spec = Just spec
                                    , state =
                                        Encode.object
                                            [ ( "a", Encode.string "new" )
                                            , ( "b", Encode.string "" )
                                            , ( "c", Encode.string "" )
                                            ]
                                    }

                                ( newModel, _ ) =
                                    Actions.checkWatchers testActionConfig oldState model
                            in
                            Expect.all
                                [ \m ->
                                    State.get "/b" m.state
                                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                        |> Expect.equal (Just "from-a")
                                , \m ->
                                    State.get "/c" m.state
                                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                        |> Expect.equal (Just "from-b")
                                ]
                                newModel

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "does not fire watchers without a spec" <|
                \_ ->
                    let
                        oldState =
                            Encode.object [ ( "x", Encode.int 1 ) ]

                        model =
                            { spec = Nothing
                            , state = Encode.object [ ( "x", Encode.int 2 ) ]
                            }

                        ( newModel, _ ) =
                            Actions.checkWatchers testActionConfig oldState model
                    in
                    State.get "/x" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                        |> Expect.equal (Just 2)
            ]
        ]
