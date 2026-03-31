module JsonRenderTest exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender
import JsonRender.State as State
import Random
import Test exposing (..)


testSeed : Random.Seed
testSeed =
    Random.initialSeed 42


validSpecJson : Encode.Value
validSpecJson =
    Encode.object
        [ ( "root", Encode.string "root-1" )
        , ( "elements"
          , Encode.object
                [ ( "root-1"
                  , Encode.object
                        [ ( "type", Encode.string "text" )
                        , ( "props"
                          , Encode.object
                                [ ( "content", Encode.string "Hello" ) ]
                          )
                        ]
                  )
                ]
          )
        ]


validSpecJsonWithState : Encode.Value
validSpecJsonWithState =
    Encode.object
        [ ( "root", Encode.string "root-1" )
        , ( "elements"
          , Encode.object
                [ ( "root-1"
                  , Encode.object
                        [ ( "type", Encode.string "text" )
                        , ( "props"
                          , Encode.object
                                [ ( "content", Encode.string "Hello" ) ]
                          )
                        ]
                  )
                ]
          )
        , ( "state"
          , Encode.object
                [ ( "name", Encode.string "Alice" )
                , ( "count", Encode.int 5 )
                ]
          )
        ]


suite : Test
suite =
    describe "JsonRender"
        [ describe "init"
            [ test "creates model with empty state and no spec" <|
                \_ ->
                    let
                        model =
                            JsonRender.init testSeed
                    in
                    Expect.all
                        [ \m -> m.spec |> Expect.equal Nothing
                        , \m ->
                            Decode.decodeValue (Decode.dict Decode.value) m.state
                                |> Result.map (\_ -> True)
                                |> Expect.equal (Ok True)
                        ]
                        model
            ]
        , describe "receiveSpec"
            [ test "decodes valid spec and updates model" <|
                \_ ->
                    let
                        model =
                            JsonRender.init testSeed

                        result =
                            JsonRender.receiveSpec validSpecJson model
                    in
                    case result of
                        Ok newModel ->
                            Expect.all
                                [ \m -> m.spec |> Expect.notEqual Nothing
                                , \m ->
                                    case m.spec of
                                        Just spec ->
                                            spec.root |> Expect.equal "root-1"

                                        Nothing ->
                                            Expect.fail "spec should be Just"
                                ]
                                newModel

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ err)
            , test "returns error on invalid JSON" <|
                \_ ->
                    let
                        model =
                            JsonRender.init testSeed

                        invalidJson =
                            Encode.object
                                [ ( "notRoot", Encode.string "bad" ) ]

                        result =
                            JsonRender.receiveSpec invalidJson model
                    in
                    case result of
                        Err _ ->
                            Expect.pass

                        Ok _ ->
                            Expect.fail "Expected Err for invalid spec JSON"
            , test "applies spec state to model" <|
                \_ ->
                    let
                        model =
                            JsonRender.init testSeed

                        result =
                            JsonRender.receiveSpec validSpecJsonWithState model
                    in
                    case result of
                        Ok newModel ->
                            Expect.all
                                [ \m ->
                                    State.get "/name" m.state
                                        |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                        |> Expect.equal (Just "Alice")
                                , \m ->
                                    State.get "/count" m.state
                                        |> Maybe.andThen (Decode.decodeValue Decode.int >> Result.toMaybe)
                                        |> Expect.equal (Just 5)
                                ]
                                newModel

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ err)
            , test "preserves existing state when spec has no state field" <|
                \_ ->
                    let
                        model =
                            { spec = Nothing
                            , state =
                                Encode.object
                                    [ ( "existing", Encode.string "keep me" ) ]
                            , seed = testSeed
                            }

                        result =
                            JsonRender.receiveSpec validSpecJson model
                    in
                    case result of
                        Ok newModel ->
                            State.get "/existing" newModel.state
                                |> Maybe.andThen (Decode.decodeValue Decode.string >> Result.toMaybe)
                                |> Expect.equal (Just "keep me")

                        Err err ->
                            Expect.fail ("Expected Ok, got Err: " ++ err)
            ]
        ]
