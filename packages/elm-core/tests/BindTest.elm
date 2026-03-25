module BindTest exposing (..)

import Dict
import Expect
import Json.Encode exposing (Value)
import JsonRender.Actions exposing (Msg(..))
import JsonRender.Bind as Bind
import Test exposing (..)


type TestAction
    = NoAction


type alias TestBindings =
    { name : Maybe (Value -> Msg TestAction)
    , age : Maybe (Value -> Msg TestAction)
    }


suite : Test
suite =
    describe "JsonRender.Bind"
        [ test "succeed returns constant value" <|
            \_ ->
                let
                    decoder =
                        Bind.succeed ()

                    result =
                        decoder Dict.empty
                in
                Expect.equal () result
        , test "bindable extracts setter when present" <|
            \_ ->
                let
                    setter =
                        \val -> SetState "/name" val

                    dict =
                        Dict.fromList [ ( "name", setter ) ]

                    decoder =
                        Bind.succeed identity
                            |> Bind.bindable "name"

                    result =
                        decoder dict
                in
                Expect.notEqual Nothing result
        , test "bindable returns Nothing when key absent" <|
            \_ ->
                let
                    decoder =
                        Bind.succeed identity
                            |> Bind.bindable "name"

                    result =
                        decoder Dict.empty
                in
                Expect.equal Nothing result
        , test "pipeline builds full bindings record" <|
            \_ ->
                let
                    nameSetter =
                        \val -> SetState "/name" val

                    dict =
                        Dict.fromList [ ( "name", nameSetter ) ]

                    decoder =
                        Bind.succeed TestBindings
                            |> Bind.bindable "name"
                            |> Bind.bindable "age"

                    result =
                        decoder dict
                in
                Expect.all
                    [ \r -> Expect.notEqual Nothing r.name
                    , \r -> Expect.equal Nothing r.age
                    ]
                    result
        ]
