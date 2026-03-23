module ActionsTest exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Actions as Actions exposing (Msg(..))
import JsonRender.State as State
import Test exposing (..)


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
                        Actions.update (SetState "/name" (Encode.string "Bob")) model
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
                        Actions.update (PushState "/items" (Encode.string "b")) model
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
                        Actions.update (RemoveState "/age") model
                in
                State.get "/age" newModel.state
                    |> Expect.equal Nothing
        , test "encodeAction produces correct JSON" <|
            \_ ->
                let
                    json =
                        Actions.encodeAction "export" (Encode.object [ ( "format", Encode.string "pdf" ) ])

                    name =
                        Decode.decodeValue (Decode.field "name" Decode.string) json
                in
                Expect.equal (Ok "export") name
        ]
