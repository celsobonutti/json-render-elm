module ActionsTest exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Actions as Actions exposing (Msg(..))
import JsonRender.State as State
import Test exposing (..)


type TestAction
    = TestPress
    | TestExport { format : String }


suite : Test
suite =
    describe "JsonRender.Actions"
        [ test "SetState updates state" <|
            \_ ->
                let
                    model =
                        { spec = Nothing
                        , state = Encode.object [ ( "name", Encode.string "Alice" ) ]
                        }

                    ( newModel, _ ) =
                        Actions.update noOpHandler (SetState "/name" (Encode.string "Bob")) model
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
                        Actions.update noOpHandler (PushState "/items" (Encode.string "b")) model
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
                        Actions.update noOpHandler (RemoveState "/age") model
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

                    handler action m =
                        case action of
                            TestExport { format } ->
                                ( { m | state = Encode.object [ ( "exported", Encode.string format ) ] }
                                , Cmd.none
                                )

                            _ ->
                                ( m, Cmd.none )

                    ( newModel, _ ) =
                        Actions.update handler (CustomAction (TestExport { format = "pdf" })) model
                in
                State.get "/exported" newModel.state
                    |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                    |> Expect.equal (Just "pdf")
        ]


noOpHandler : TestAction -> Actions.Model -> ( Actions.Model, Cmd (Msg TestAction) )
noOpHandler _ model =
    ( model, Cmd.none )
