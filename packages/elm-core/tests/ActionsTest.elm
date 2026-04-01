module ActionsTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Actions as Actions exposing (Msg(..), ResolvedAction(..))
import JsonRender.Spec exposing (EventHandler(..))
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.State as State
import Random
import Test exposing (..)
import UUID


type TestAction
    = TestPress
    | TestExport { format : String }


testActionConfig : Actions.ActionConfig TestAction
testActionConfig =
    { handleAction = \_ model -> ( model, Cmd.none )
    , decodeAction = decodeTestAction
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


testModel : Encode.Value -> Actions.Model
testModel state =
    { spec = Nothing, state = state, seed = Random.initialSeed 42, validationState = Dict.empty }


executeAction : String -> List ( String, PropValue ) -> Msg TestAction
executeAction name params =
    Actions.ExecuteAction (SingleAction { action = name, params = Dict.fromList params, preventDefault = False }) Nothing


executeChain : List { action : String, params : List ( String, PropValue ) } -> Msg TestAction
executeChain bindings =
    Actions.ExecuteAction (ChainedActions (List.map (\b -> { action = b.action, params = Dict.fromList b.params, preventDefault = False }) bindings)) Nothing


setStateMsg : String -> PropValue -> Msg TestAction
setStateMsg path value =
    executeAction "setState" [ ( "statePath", StringValue path ), ( "value", value ) ]


pushStateMsg : String -> PropValue -> Msg TestAction
pushStateMsg path value =
    executeAction "pushState" [ ( "statePath", StringValue path ), ( "value", value ) ]


removeStateMsg : String -> Msg TestAction
removeStateMsg path =
    executeAction "removeState" [ ( "statePath", StringValue path ) ]


suite : Test
suite =
    describe "JsonRender.Actions"
        [ describe "built-in actions via ExecuteAction"
            [ test "setState updates state" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [ ( "name", Encode.string "Alice" ) ])

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (setStateMsg "/name" (StringValue "Bob")) model
                    in
                    State.get "/name" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Bob")
            , test "pushState appends to array" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [ ( "items", Encode.list Encode.string [ "a" ] ) ])

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (pushStateMsg "/items" (StringValue "b")) model
                    in
                    State.get "/items/1" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "b")
            , test "removeState removes from state" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "name", Encode.string "Alice" )
                                    , ( "age", Encode.int 30 )
                                    ]
                                )

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (removeStateMsg "/age") model
                    in
                    State.get "/age" newModel.state
                        |> Expect.equal Nothing
            , test "custom action delegates to handler" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [])

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
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty config (executeAction "export" [ ( "format", StringValue "pdf" ) ]) model
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
                            testModel (Encode.object [ ( "clicked", Encode.bool False ) ])

                        binding =
                            { action = "setState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/clicked" )
                                    , ( "value", BoolValue True )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/clicked" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.bool >> Result.toMaybe)
                        |> Expect.equal (Just True)
            , test "ExecuteAction with pushState appends to array" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list Encode.string [ "first" ] ) ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value", StringValue "second" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/items/1" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "second")
            , test "ExecuteAction with removeState removes from state" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "name", Encode.string "Alice" )
                                    , ( "temp", Encode.string "remove me" )
                                    ]
                                )

                        binding =
                            { action = "removeState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/temp" ) ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
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
                            testModel (Encode.object [])

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
                            }

                        binding =
                            { action = "export"
                            , params =
                                Dict.fromList
                                    [ ( "format", StringValue "csv" ) ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty exportConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/exported" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "csv")
            , test "ExecuteAction with unknown action name does not change state" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [ ( "x", Encode.int 1 ) ])

                        binding =
                            { action = "nonexistentAction"
                            , params = Dict.empty
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/x" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                        |> Expect.equal (Just 1)
            , test "ExecuteAction with $state expression resolves against current state" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "source", Encode.string "hello" )
                                    , ( "items", Encode.list Encode.string [] )
                                    ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value", StateExpr "/source" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/items/0" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "hello")
            ]
        , describe "chained action execution (chained actions)"
            [ test "chained actions executes two actions in sequence" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "loading", Encode.bool False )
                                    , ( "items", Encode.list Encode.string [] )
                                    ]
                                )

                        bindings =
                            [ { action = "setState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/loading" )
                                        , ( "value", BoolValue True )
                                        ]
                              , preventDefault = False
                              }
                            , { action = "pushState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/items" )
                                        , ( "value", StringValue "new item" )
                                        ]
                              , preventDefault = False
                              }
                            ]

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (ChainedActions bindings) Nothing) model
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
            , test "chained actions second action sees first action mutations via $state" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "input", Encode.string "Buy milk" )
                                    , ( "items", Encode.list Encode.string [] )
                                    ]
                                )

                        bindings =
                            [ -- First: push current input value to items
                              { action = "pushState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/items" )
                                        , ( "value", StateExpr "/input" )
                                        ]
                              , preventDefault = False
                              }
                            , -- Second: clear input
                              { action = "setState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/input" )
                                        , ( "value", StringValue "" )
                                        ]
                              , preventDefault = False
                              }
                            ]

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (ChainedActions bindings) Nothing) model
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
            , test "chained actions with empty list does not change state" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [ ( "x", Encode.int 42 ) ])

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (ChainedActions []) Nothing) model
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
                            testModel (Encode.object [ ( "x", Encode.int 1 ) ])

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ActionError "No handler for event: tap") model
                    in
                    State.get "/x" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                        |> Expect.equal (Just 1)
            ]
        , describe "BindingUpdate"
            [ test "BindingUpdate sets state at path" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [ ( "name", Encode.string "Alice" ) ])

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (BindingUpdate "/name" (Encode.string "Bob")) model
                    in
                    State.get "/name" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Bob")
            , test "BindingUpdate creates new path" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [])

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (BindingUpdate "/newKey" (Encode.int 99)) model
                    in
                    State.get "/newKey" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                        |> Expect.equal (Just 99)
            ]
        , describe "pushState $id auto-generation"
            [ test "pushState with $id in value replaces with UUID" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list identity [] ) ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value"
                                      , ObjectValue
                                            (Dict.fromList
                                                [ ( "id", StringValue "$id" )
                                                , ( "title", StringValue "hello" )
                                                ]
                                            )
                                      )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    case State.get "/items/0/id" newModel.state of
                        Just idVal ->
                            case Decode.decodeValue Decode.string idVal of
                                Ok idStr ->
                                    case UUID.fromString idStr of
                                        Ok _ ->
                                            Expect.pass

                                        Err _ ->
                                            Expect.fail ("Expected valid UUID, got: " ++ idStr)

                                Err _ ->
                                    Expect.fail "id should be a string"

                        Nothing ->
                            Expect.fail "pushed item should have id field"
            , test "multiple $id occurrences get different UUIDs" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list identity [] ) ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value"
                                      , ObjectValue
                                            (Dict.fromList
                                                [ ( "id", StringValue "$id" )
                                                , ( "ref", StringValue "$id" )
                                                ]
                                            )
                                      )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model

                        getId path =
                            State.get path newModel.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                    in
                    case ( getId "/items/0/id", getId "/items/0/ref" ) of
                        ( Just id1, Just id2 ) ->
                            Expect.notEqual id1 id2

                        _ ->
                            Expect.fail "both id and ref should be strings"
            , test "$id in nested objects is substituted" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list identity [] ) ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value"
                                      , ObjectValue
                                            (Dict.fromList
                                                [ ( "meta"
                                                  , ObjectValue
                                                        (Dict.fromList
                                                            [ ( "uid", StringValue "$id" ) ]
                                                        )
                                                  )
                                                ]
                                            )
                                      )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    case State.get "/items/0/meta/uid" newModel.state of
                        Just idVal ->
                            case Decode.decodeValue Decode.string idVal of
                                Ok idStr ->
                                    case UUID.fromString idStr of
                                        Ok _ ->
                                            Expect.pass

                                        Err _ ->
                                            Expect.fail ("Expected valid UUID, got: " ++ idStr)

                                Err _ ->
                                    Expect.fail "uid should be a string"

                        Nothing ->
                            Expect.fail "pushed item should have meta/uid field"
            , test "$id in arrays is substituted" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list identity [] ) ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value"
                                      , ListValue
                                            [ StringValue "$id"
                                            , StringValue "static"
                                            ]
                                      )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    case State.get "/items/0/0" newModel.state of
                        Just idVal ->
                            case Decode.decodeValue Decode.string idVal of
                                Ok idStr ->
                                    Expect.notEqual "$id" idStr

                                Err _ ->
                                    Expect.fail "first element should be a string"

                        Nothing ->
                            Expect.fail "pushed item should have element at index 0"
            , test "non-$id strings are not substituted" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list identity [] ) ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value"
                                      , ObjectValue
                                            (Dict.fromList
                                                [ ( "name", StringValue "Alice" ) ]
                                            )
                                      )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/items/0/name" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Alice")
            , test "setState ignores $id in value" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [])

                        binding =
                            { action = "setState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/name" )
                                    , ( "value", StringValue "$id" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/name" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "$id")
            ]
        , describe "pushState clearStatePath"
            [ test "clearStatePath resets path to empty string after push" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "input", Encode.string "Buy milk" )
                                    , ( "items", Encode.list identity [] )
                                    ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value", StateExpr "/input" )
                                    , ( "clearStatePath", StringValue "/input" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
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
            , test "pushState without clearStatePath does not clear anything" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "input", Encode.string "Buy milk" )
                                    , ( "items", Encode.list identity [] )
                                    ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value", StateExpr "/input" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/input" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Buy milk")
            , test "clearStatePath with $state expression resolves the path to clear" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "input", Encode.string "Buy milk" )
                                    , ( "items", Encode.list identity [] )
                                    , ( "clearTarget", Encode.string "/input" )
                                    ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value", StateExpr "/input" )
                                    , ( "clearStatePath", StateExpr "/clearTarget" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
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
            , test "clearStatePath is ignored on non-pushState actions" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "input", Encode.string "keep me" )
                                    , ( "flag", Encode.bool False )
                                    ]
                                )

                        binding =
                            { action = "setState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/flag" )
                                    , ( "value", BoolValue True )
                                    , ( "clearStatePath", StringValue "/input" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    State.get "/input" newModel.state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "keep me")
            ]
        , describe "middleware integration"
            [ test "pushState with $id + clearStatePath in one action" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "input", Encode.string "Buy milk" )
                                    , ( "todos", Encode.list identity [] )
                                    ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/todos" )
                                    , ( "value"
                                      , ObjectValue
                                            (Dict.fromList
                                                [ ( "id", StringValue "$id" )
                                                , ( "title", StateExpr "/input" )
                                                ]
                                            )
                                      )
                                    , ( "clearStatePath", StringValue "/input" )
                                    ]
                            , preventDefault = False
                            }

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (SingleAction binding) Nothing) model
                    in
                    Expect.all
                        [ \m ->
                            State.get "/todos/0/title" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "Buy milk")
                        , \m ->
                            case State.get "/todos/0/id" m.state of
                                Just idVal ->
                                    case Decode.decodeValue Decode.string idVal of
                                        Ok idStr ->
                                            case UUID.fromString idStr of
                                                Ok _ ->
                                                    Expect.pass

                                                Err _ ->
                                                    Expect.fail ("Expected UUID, got: " ++ idStr)

                                        Err _ ->
                                            Expect.fail "id should be a string"

                                Nothing ->
                                    Expect.fail "todo should have id"
                        , \m ->
                            State.get "/input" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "")
                        ]
                        newModel
            , test "two pushState actions in chain produce different UUIDs" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list identity [] ) ]
                                )

                        bindings =
                            [ { action = "pushState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/items" )
                                        , ( "value"
                                          , ObjectValue
                                                (Dict.fromList
                                                    [ ( "id", StringValue "$id" ) ]
                                                )
                                          )
                                        ]
                              , preventDefault = False
                              }
                            , { action = "pushState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/items" )
                                        , ( "value"
                                          , ObjectValue
                                                (Dict.fromList
                                                    [ ( "id", StringValue "$id" ) ]
                                                )
                                          )
                                        ]
                              , preventDefault = False
                              }
                            ]

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (ChainedActions bindings) Nothing) model

                        getId path =
                            State.get path newModel.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                    in
                    case ( getId "/items/0/id", getId "/items/1/id" ) of
                        ( Just id1, Just id2 ) ->
                            Expect.notEqual id1 id2

                        _ ->
                            Expect.fail "both items should have id strings"
            , test "chained actions: pushState with clearStatePath, then setState sees cleared state" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "input", Encode.string "Buy milk" )
                                    , ( "todos", Encode.list identity [] )
                                    , ( "lastInput", Encode.string "" )
                                    ]
                                )

                        bindings =
                            [ { action = "pushState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/todos" )
                                        , ( "value", StateExpr "/input" )
                                        , ( "clearStatePath", StringValue "/input" )
                                        ]
                              , preventDefault = False
                              }
                            , { action = "setState"
                              , params =
                                    Dict.fromList
                                        [ ( "statePath", StringValue "/lastInput" )
                                        , ( "value", StateExpr "/input" )
                                        ]
                              , preventDefault = False
                              }
                            ]

                        ( newModel, _ ) =
                            Actions.update Dict.empty testActionConfig (ExecuteAction (ChainedActions bindings) Nothing) model
                    in
                    Expect.all
                        [ \m ->
                            State.get "/todos/0" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "Buy milk")
                        , \m ->
                            State.get "/input" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "")
                        , \m ->
                            -- The second action reads /input AFTER clearStatePath ran
                            State.get "/lastInput" m.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "")
                        ]
                        newModel
            ]
        , describe "resolveBinding"
            [ test "resolves setState binding" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [ ( "name", Encode.string "Alice" ) ])

                        binding =
                            { action = "setState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/name" )
                                    , ( "value", StringValue "Bob" )
                                    ]
                            , preventDefault = False
                            }

                        result =
                            Actions.resolveBinding Dict.empty testActionConfig binding Nothing model
                    in
                    case result of
                        Ok ( SetState { path, value }, _ ) ->
                            Expect.all
                                [ \_ -> Expect.equal "/name" path
                                , \_ ->
                                    Decode.decodeValue Decode.string value
                                        |> Result.toMaybe
                                        |> Expect.equal (Just "Bob")
                                ]
                                ()

                        Ok ( other, _ ) ->
                            Expect.fail ("Expected SetState, got: " ++ Debug.toString other)

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ err)
            , test "resolves pushState with $id and clearPath" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list identity [] )
                                    , ( "input", Encode.string "hello" )
                                    ]
                                )

                        binding =
                            { action = "pushState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "value"
                                      , ObjectValue
                                            (Dict.fromList
                                                [ ( "id", StringValue "$id" )
                                                , ( "text", StateExpr "/input" )
                                                ]
                                            )
                                      )
                                    , ( "clearStatePath", StringValue "/input" )
                                    ]
                            , preventDefault = False
                            }

                        result =
                            Actions.resolveBinding Dict.empty testActionConfig binding Nothing model
                    in
                    case result of
                        Ok ( PushState { path, clearPath, value }, _ ) ->
                            Expect.all
                                [ \_ -> Expect.equal "/items" path
                                , \_ -> Expect.equal (Just "/input") clearPath
                                , \_ ->
                                    -- The $id should have been substituted (not "$id")
                                    case Decode.decodeValue (Decode.field "id" Decode.string) value of
                                        Ok idStr ->
                                            Expect.notEqual "$id" idStr

                                        Err _ ->
                                            Expect.fail "value should have an id field"
                                ]
                                ()

                        Ok ( other, _ ) ->
                            Expect.fail ("Expected PushState, got: " ++ Debug.toString other)

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ err)
            , test "resolves removeState with index" <|
                \_ ->
                    let
                        model =
                            testModel
                                (Encode.object
                                    [ ( "items", Encode.list Encode.string [ "a", "b", "c" ] ) ]
                                )

                        binding =
                            { action = "removeState"
                            , params =
                                Dict.fromList
                                    [ ( "statePath", StringValue "/items" )
                                    , ( "index", IntValue 2 )
                                    ]
                            , preventDefault = False
                            }

                        result =
                            Actions.resolveBinding Dict.empty testActionConfig binding Nothing model
                    in
                    case result of
                        Ok ( RemoveState { path, index }, _ ) ->
                            Expect.all
                                [ \_ -> Expect.equal "/items" path
                                , \_ -> Expect.equal (Just 2) index
                                ]
                                ()

                        Ok ( other, _ ) ->
                            Expect.fail ("Expected RemoveState, got: " ++ Debug.toString other)

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ err)
            , test "resolves custom action" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [])

                        binding =
                            { action = "press"
                            , params = Dict.empty
                            , preventDefault = False
                            }

                        result =
                            Actions.resolveBinding Dict.empty testActionConfig binding Nothing model
                    in
                    case result of
                        Ok ( CustomAction TestPress, _ ) ->
                            Expect.pass

                        Ok ( other, _ ) ->
                            Expect.fail ("Expected CustomAction TestPress, got: " ++ Debug.toString other)

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ err)
            , test "returns error for missing statePath" <|
                \_ ->
                    let
                        model =
                            testModel (Encode.object [])

                        binding =
                            { action = "setState"
                            , params =
                                Dict.fromList
                                    [ ( "value", StringValue "hello" ) ]
                            , preventDefault = False
                            }

                        result =
                            Actions.resolveBinding Dict.empty testActionConfig binding Nothing model
                    in
                    case result of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Expected Err for missing statePath"
            ]
        ]
