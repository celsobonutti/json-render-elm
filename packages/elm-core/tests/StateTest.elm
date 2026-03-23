module StateTest exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.State as State
import Test exposing (..)


suite : Test
suite =
    describe "JsonRender.State"
        [ describe "get"
            [ test "gets a top-level key" <|
                \_ ->
                    let
                        state =
                            Encode.object [ ( "name", Encode.string "Alice" ) ]
                    in
                    State.get "/name" state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Alice")
            , test "gets a nested key" <|
                \_ ->
                    let
                        state =
                            Encode.object
                                [ ( "user"
                                  , Encode.object [ ( "name", Encode.string "Bob" ) ]
                                  )
                                ]
                    in
                    State.get "/user/name" state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Bob")
            , test "gets an array element by index" <|
                \_ ->
                    let
                        state =
                            Encode.object
                                [ ( "items"
                                  , Encode.list Encode.string [ "a", "b", "c" ]
                                  )
                                ]
                    in
                    State.get "/items/1" state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "b")
            , test "returns Nothing for missing path" <|
                \_ ->
                    let
                        state =
                            Encode.object [ ( "name", Encode.string "Alice" ) ]
                    in
                    State.get "/age" state
                        |> Expect.equal Nothing
            , test "returns root for empty path" <|
                \_ ->
                    let
                        state =
                            Encode.string "hello"
                    in
                    State.get "" state
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "hello")
            ]
        , describe "set"
            [ test "sets a top-level key" <|
                \_ ->
                    let
                        state =
                            Encode.object [ ( "name", Encode.string "Alice" ) ]

                        result =
                            State.set "/name" (Encode.string "Bob") state
                    in
                    State.get "/name" result
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Bob")
            , test "sets a nested key" <|
                \_ ->
                    let
                        state =
                            Encode.object
                                [ ( "user"
                                  , Encode.object [ ( "name", Encode.string "Alice" ) ]
                                  )
                                ]

                        result =
                            State.set "/user/name" (Encode.string "Bob") state
                    in
                    State.get "/user/name" result
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "Bob")
            ]
        , describe "push"
            [ test "appends to an array" <|
                \_ ->
                    let
                        state =
                            Encode.object
                                [ ( "items"
                                  , Encode.list Encode.string [ "a", "b" ]
                                  )
                                ]

                        result =
                            State.push "/items" (Encode.string "c") state
                    in
                    State.get "/items/2" result
                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                        |> Expect.equal (Just "c")
            ]
        , describe "remove"
            [ test "removes a key from an object" <|
                \_ ->
                    let
                        state =
                            Encode.object
                                [ ( "name", Encode.string "Alice" )
                                , ( "age", Encode.int 30 )
                                ]

                        result =
                            State.remove "/age" state
                    in
                    State.get "/age" result
                        |> Expect.equal Nothing
            ]
        ]
