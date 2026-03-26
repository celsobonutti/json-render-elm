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
                    |> Expect.equal (Ok True)
        , test "Truthy returns false for falsy value" <|
            \_ ->
                Visibility.evaluate state Nothing (Truthy "/count")
                    |> Expect.equal (Ok False)
        , test "Equals matches string" <|
            \_ ->
                Visibility.evaluate state Nothing (Equals "/role" (StringValue "admin"))
                    |> Expect.equal (Ok True)
        , test "NotEquals works" <|
            \_ ->
                Visibility.evaluate state Nothing (NotEquals "/role" (StringValue "user"))
                    |> Expect.equal (Ok True)
        , test "Not inverts" <|
            \_ ->
                Visibility.evaluate state Nothing (Not (Truthy "/isAdmin"))
                    |> Expect.equal (Ok False)
        , test "And requires all true" <|
            \_ ->
                Visibility.evaluate state Nothing
                    (And [ Truthy "/isAdmin", Equals "/role" (StringValue "admin") ])
                    |> Expect.equal (Ok True)
        , test "Or requires any true" <|
            \_ ->
                Visibility.evaluate state Nothing
                    (Or [ Truthy "/count", Truthy "/isAdmin" ])
                    |> Expect.equal (Ok True)
        , test "decodes truthy condition from JSON" <|
            \_ ->
                let
                    json =
                        """{"$state": "/isAdmin"}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Expect.equal (Truthy "/isAdmin") condition

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes falsy (not) condition from JSON" <|
            \_ ->
                let
                    json =
                        """{"$state": "/isAdmin", "not": true}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Expect.equal (Not (Truthy "/isAdmin")) condition

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes and evaluates eq condition from JSON" <|
            \_ ->
                let
                    json =
                        """{"$state": "/role", "eq": "admin"}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes and evaluates neq condition from JSON" <|
            \_ ->
                let
                    json =
                        """{"$state": "/role", "neq": "user"}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes and evaluates $or from JSON" <|
            \_ ->
                let
                    json =
                        """{"$or": [{"$state": "/count"}, {"$state": "/isAdmin"}]}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes and evaluates $and from JSON" <|
            \_ ->
                let
                    json =
                        """{"$and": [{"$state": "/isAdmin"}, {"$state": "/role", "eq": "admin"}]}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes implicit AND (list of conditions)" <|
            \_ ->
                let
                    json =
                        """[{"$state": "/isAdmin"}, {"$state": "/role", "eq": "admin"}]"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes true literal" <|
            \_ ->
                case Decode.decodeString Visibility.decoder "true" of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok True)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes false literal" <|
            \_ ->
                case Decode.decodeString Visibility.decoder "false" of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok False)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , describe "evaluate returns Result"
            [ test "truthy state returns Ok True" <|
                \_ ->
                    let
                        s =
                            Encode.object [ ( "show", Encode.bool True ) ]
                    in
                    Expect.equal (Ok True)
                        (Visibility.evaluate s Nothing (Truthy "/show"))
            , test "falsy state returns Ok False" <|
                \_ ->
                    let
                        s =
                            Encode.object [ ( "show", Encode.bool False ) ]
                    in
                    Expect.equal (Ok False)
                        (Visibility.evaluate s Nothing (Truthy "/show"))
            ]
        , test "decodes eq with not inversion" <|
            \_ ->
                let
                    json =
                        """{"$state": "/role", "eq": "admin", "not": true}"""
                in
                case Decode.decodeString Visibility.decoder json of
                    Ok condition ->
                        Visibility.evaluate state Nothing condition
                            |> Expect.equal (Ok False)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]
