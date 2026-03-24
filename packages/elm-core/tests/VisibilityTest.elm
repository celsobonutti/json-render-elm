module VisibilityTest exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Visibility as Visibility exposing (VisibilityCondition(..))
import Test exposing (..)


state : Encode.Value
state =
    Encode.object
        [ ( "isAdmin", Encode.bool True )
        , ( "role", Encode.string "admin" )
        , ( "count", Encode.int 0 )
        ]


suite : Test
suite =
    describe "JsonRender.Visibility"
        [ test "Truthy returns true for truthy value" <|
            \_ ->
                Visibility.evaluate state Nothing (Truthy "/isAdmin")
                    |> Expect.equal True
        , test "Truthy returns false for falsy value" <|
            \_ ->
                Visibility.evaluate state Nothing (Truthy "/count")
                    |> Expect.equal False
        , test "Equals matches string" <|
            \_ ->
                Visibility.evaluate state Nothing (Equals "/role" (StringValue "admin"))
                    |> Expect.equal True
        , test "NotEquals works" <|
            \_ ->
                Visibility.evaluate state Nothing (NotEquals "/role" (StringValue "user"))
                    |> Expect.equal True
        , test "Not inverts" <|
            \_ ->
                Visibility.evaluate state Nothing (Not (Truthy "/isAdmin"))
                    |> Expect.equal False
        , test "And requires all true" <|
            \_ ->
                Visibility.evaluate state Nothing
                    (And [ Truthy "/isAdmin", Equals "/role" (StringValue "admin") ])
                    |> Expect.equal True
        , test "Or requires any true" <|
            \_ ->
                Visibility.evaluate state Nothing
                    (Or [ Truthy "/count", Truthy "/isAdmin" ])
                    |> Expect.equal True
        , test "decodes truthy condition from JSON" <|
            \_ ->
                let
                    json =
                        """{"truthy": "/isAdmin"}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Expect.equal (Truthy "/isAdmin") condition

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes and evaluates equals condition from JSON" <|
            \_ ->
                let
                    json =
                        """{"equals": {"path": "/role", "value": "admin"}}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal True

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes and evaluates compound condition from JSON" <|
            \_ ->
                let
                    json =
                        """{"and": [{"truthy": "/isAdmin"}, {"equals": {"path": "/role", "value": "admin"}}]}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal True

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]
