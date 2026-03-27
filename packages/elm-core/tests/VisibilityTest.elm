module VisibilityTest exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Visibility as Visibility exposing (VisibilityCondition(..))
import Test exposing (..)


eval : Encode.Value -> Maybe Visibility.RepeatContext -> String -> Result String Bool
eval state repeatCtx json =
    case Decode.decodeString Visibility.decoder json of
        Ok condition ->
            Visibility.evaluate state repeatCtx condition

        Err err ->
            Err ("Decode error: " ++ Decode.errorToString err)


suite : Test
suite =
    describe "JsonRender.Visibility"
        [ truthinessSuite
        , negationSuite
        , equalitySuite
        , inequalitySuite
        , numericSuite
        , dynamicRefSuite
        , implicitAndSuite
        , andSuite
        , orSuite
        , itemSuite
        , indexSuite
        ]


truthinessSuite : Test
truthinessSuite =
    describe "truthiness ($state only)"
        [ test "returns true when state path is truthy (boolean)" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool True ) ]) Nothing
                    """{"$state": "/isAdmin"}"""
                    |> Expect.equal (Ok True)
        , test "returns true when state path is truthy (number)" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count"}"""
                    |> Expect.equal (Ok True)
        , test "returns true when state path is truthy (string)" <|
            \_ ->
                eval (Encode.object [ ( "name", Encode.string "Alice" ) ]) Nothing
                    """{"$state": "/name"}"""
                    |> Expect.equal (Ok True)
        , test "returns false when state path is falsy (boolean)" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool False ) ]) Nothing
                    """{"$state": "/isAdmin"}"""
                    |> Expect.equal (Ok False)
        , test "returns false when state path is falsy (zero)" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 0 ) ]) Nothing
                    """{"$state": "/count"}"""
                    |> Expect.equal (Ok False)
        , test "returns false when state path is falsy (empty string)" <|
            \_ ->
                eval (Encode.object [ ( "name", Encode.string "" ) ]) Nothing
                    """{"$state": "/name"}"""
                    |> Expect.equal (Ok False)
        , test "returns false when state path is undefined" <|
            \_ ->
                eval (Encode.object []) Nothing
                    """{"$state": "/nothing"}"""
                    |> Expect.equal (Ok False)
        , test "returns false for missing path" <|
            \_ ->
                eval (Encode.object [ ( "other", Encode.bool True ) ]) Nothing
                    """{"$state": "/nonexistent"}"""
                    |> Expect.equal (Ok False)
        ]


negationSuite : Test
negationSuite =
    describe "negation ($state + not)"
        [ test "returns false when state path is truthy" <|
            \_ ->
                eval (Encode.object [ ( "visible", Encode.bool True ) ]) Nothing
                    """{"$state": "/visible", "not": true}"""
                    |> Expect.equal (Ok False)
        , test "returns true when state path is falsy" <|
            \_ ->
                eval (Encode.object [ ( "visible", Encode.bool False ) ]) Nothing
                    """{"$state": "/visible", "not": true}"""
                    |> Expect.equal (Ok True)
        , test "not inverts an eq condition" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        eval (Encode.object [ ( "tab", Encode.string "home" ) ]) Nothing
                            """{"$state": "/tab", "eq": "home", "not": true}"""
                            |> Expect.equal (Ok False)
                    , \_ ->
                        eval (Encode.object [ ( "tab", Encode.string "settings" ) ]) Nothing
                            """{"$state": "/tab", "eq": "home", "not": true}"""
                            |> Expect.equal (Ok True)
                    ]
                    ()
        , test "not inverts a gt condition" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        eval (Encode.object [ ( "count", Encode.int 10 ) ]) Nothing
                            """{"$state": "/count", "gt": 5, "not": true}"""
                            |> Expect.equal (Ok False)
                    , \_ ->
                        eval (Encode.object [ ( "count", Encode.int 3 ) ]) Nothing
                            """{"$state": "/count", "gt": 5, "not": true}"""
                            |> Expect.equal (Ok True)
                    ]
                    ()
        ]


equalitySuite : Test
equalitySuite =
    describe "equality ($state + eq)"
        [ test "returns true when values match (number)" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "eq": 5}"""
                    |> Expect.equal (Ok True)
        , test "returns false when values do not match" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "eq": 10}"""
                    |> Expect.equal (Ok False)
        , test "returns true when values match (string)" <|
            \_ ->
                eval (Encode.object [ ( "tab", Encode.string "home" ) ]) Nothing
                    """{"$state": "/tab", "eq": "home"}"""
                    |> Expect.equal (Ok True)
        , test "supports state-to-state comparison" <|
            \_ ->
                eval (Encode.object [ ( "a", Encode.int 42 ), ( "b", Encode.int 42 ) ]) Nothing
                    """{"$state": "/a", "eq": {"$state": "/b"}}"""
                    |> Expect.equal (Ok True)
        , test "state-to-state comparison fails when different" <|
            \_ ->
                eval (Encode.object [ ( "a", Encode.int 1 ), ( "b", Encode.int 2 ) ]) Nothing
                    """{"$state": "/a", "eq": {"$state": "/b"}}"""
                    |> Expect.equal (Ok False)
        ]


inequalitySuite : Test
inequalitySuite =
    describe "inequality ($state + neq)"
        [ test "returns true when values differ" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "neq": 10}"""
                    |> Expect.equal (Ok True)
        , test "returns false when values are equal" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "neq": 5}"""
                    |> Expect.equal (Ok False)
        ]


numericSuite : Test
numericSuite =
    describe "numeric comparisons"
        [ test "gt: returns true when greater" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "gt": 3}"""
                    |> Expect.equal (Ok True)
        , test "gt: returns false when less" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 2 ) ]) Nothing
                    """{"$state": "/count", "gt": 3}"""
                    |> Expect.equal (Ok False)
        , test "gt: returns false when equal" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "gt": 5}"""
                    |> Expect.equal (Ok False)
        , test "gte: returns true when equal" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "gte": 5}"""
                    |> Expect.equal (Ok True)
        , test "gte: returns true when greater" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 6 ) ]) Nothing
                    """{"$state": "/count", "gte": 5}"""
                    |> Expect.equal (Ok True)
        , test "gte: returns false when less" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 4 ) ]) Nothing
                    """{"$state": "/count", "gte": 5}"""
                    |> Expect.equal (Ok False)
        , test "lt: returns true when less" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 3 ) ]) Nothing
                    """{"$state": "/count", "lt": 5}"""
                    |> Expect.equal (Ok True)
        , test "lt: returns false when greater" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 7 ) ]) Nothing
                    """{"$state": "/count", "lt": 5}"""
                    |> Expect.equal (Ok False)
        , test "lt: returns false when equal" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "lt": 5}"""
                    |> Expect.equal (Ok False)
        , test "lte: returns true when equal" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "lte": 5}"""
                    |> Expect.equal (Ok True)
        , test "lte: returns true when less" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 4 ) ]) Nothing
                    """{"$state": "/count", "lte": 5}"""
                    |> Expect.equal (Ok True)
        , test "lte: returns false when greater" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 6 ) ]) Nothing
                    """{"$state": "/count", "lte": 5}"""
                    |> Expect.equal (Ok False)
        , test "returns false for non-numeric values" <|
            \_ ->
                eval (Encode.object [ ( "name", Encode.string "Alice" ) ]) Nothing
                    """{"$state": "/name", "gt": 5}"""
                    |> Expect.equal (Ok False)
        ]


dynamicRefSuite : Test
dynamicRefSuite =
    describe "dynamic path references in comparison"
        [ test "eq with $state reference on right" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 5 ), ( "limit", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "eq": {"$state": "/limit"}}"""
                    |> Expect.equal (Ok True)
        , test "lt with $state reference on right" <|
            \_ ->
                eval (Encode.object [ ( "count", Encode.int 3 ), ( "limit", Encode.int 5 ) ]) Nothing
                    """{"$state": "/count", "lt": {"$state": "/limit"}}"""
                    |> Expect.equal (Ok True)
        ]


implicitAndSuite : Test
implicitAndSuite =
    describe "array (implicit AND)"
        [ test "returns true when all conditions are true" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool True ), ( "tab", Encode.string "settings" ) ]) Nothing
                    """[{"$state": "/isAdmin"}, {"$state": "/tab", "eq": "settings"}]"""
                    |> Expect.equal (Ok True)
        , test "returns false when one condition is false" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "tab", Encode.string "settings" ) ]) Nothing
                    """[{"$state": "/isAdmin"}, {"$state": "/tab", "eq": "settings"}]"""
                    |> Expect.equal (Ok False)
        , test "returns false when all conditions are false" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "tab", Encode.string "home" ) ]) Nothing
                    """[{"$state": "/isAdmin"}, {"$state": "/tab", "eq": "settings"}]"""
                    |> Expect.equal (Ok False)
        ]


andSuite : Test
andSuite =
    describe "$and condition (explicit AND)"
        [ test "returns true when all children are true" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool True ), ( "tab", Encode.string "settings" ) ]) Nothing
                    """{"$and": [{"$state": "/isAdmin"}, {"$state": "/tab", "eq": "settings"}]}"""
                    |> Expect.equal (Ok True)
        , test "returns false when one child is false" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "tab", Encode.string "settings" ) ]) Nothing
                    """{"$and": [{"$state": "/isAdmin"}, {"$state": "/tab", "eq": "settings"}]}"""
                    |> Expect.equal (Ok False)
        , test "returns false when all children are false" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "tab", Encode.string "home" ) ]) Nothing
                    """{"$and": [{"$state": "/isAdmin"}, {"$state": "/tab", "eq": "settings"}]}"""
                    |> Expect.equal (Ok False)
        , test "supports nested $or inside $and" <|
            \_ ->
                let
                    json =
                        """{"$and": [{"$or": [{"$state": "/isAdmin"}, {"$state": "/isModerator"}]}, {"$state": "/tab", "eq": "settings"}]}"""
                in
                Expect.all
                    [ \_ ->
                        eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "isModerator", Encode.bool True ), ( "tab", Encode.string "settings" ) ]) Nothing json
                            |> Expect.equal (Ok True)
                    , \_ ->
                        eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "isModerator", Encode.bool False ), ( "tab", Encode.string "settings" ) ]) Nothing json
                            |> Expect.equal (Ok False)
                    ]
                    ()
        , test "supports booleans inside $and" <|
            \_ ->
                let
                    state =
                        Encode.object [ ( "ok", Encode.bool True ) ]
                in
                Expect.all
                    [ \_ -> eval state Nothing """{"$and": [true, {"$state": "/ok"}]}""" |> Expect.equal (Ok True)
                    , \_ -> eval state Nothing """{"$and": [false, {"$state": "/ok"}]}""" |> Expect.equal (Ok False)
                    ]
                    ()
        ]


orSuite : Test
orSuite =
    describe "$or condition"
        [ test "returns true when at least one child is true" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "isModerator", Encode.bool True ) ]) Nothing
                    """{"$or": [{"$state": "/isAdmin"}, {"$state": "/isModerator"}]}"""
                    |> Expect.equal (Ok True)
        , test "returns true when all children are true" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool True ), ( "isModerator", Encode.bool True ) ]) Nothing
                    """{"$or": [{"$state": "/isAdmin"}, {"$state": "/isModerator"}]}"""
                    |> Expect.equal (Ok True)
        , test "returns false when all children are false" <|
            \_ ->
                eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "isModerator", Encode.bool False ) ]) Nothing
                    """{"$or": [{"$state": "/isAdmin"}, {"$state": "/isModerator"}]}"""
                    |> Expect.equal (Ok False)
        , test "supports nested arrays (AND inside OR)" <|
            \_ ->
                let
                    json =
                        """{"$or": [[{"$state": "/isAdmin"}, {"$state": "/tab", "eq": "settings"}], {"$state": "/isSuperUser"}]}"""
                in
                Expect.all
                    [ \_ ->
                        eval (Encode.object [ ( "isAdmin", Encode.bool True ), ( "tab", Encode.string "settings" ), ( "isSuperUser", Encode.bool False ) ]) Nothing json
                            |> Expect.equal (Ok True)
                    , \_ ->
                        eval (Encode.object [ ( "isAdmin", Encode.bool False ), ( "tab", Encode.string "settings" ), ( "isSuperUser", Encode.bool False ) ]) Nothing json
                            |> Expect.equal (Ok False)
                    ]
                    ()
        , test "supports booleans inside $or" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        eval (Encode.object [ ( "ok", Encode.bool True ) ]) Nothing
                            """{"$or": [false, {"$state": "/ok"}]}"""
                            |> Expect.equal (Ok True)
                    , \_ ->
                        eval (Encode.object []) Nothing
                            """{"$or": [false, false]}"""
                            |> Expect.equal (Ok False)
                    ]
                    ()
        ]


itemSuite : Test
itemSuite =
    describe "$item conditions"
        [ test "$item truthiness check" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.object [ ( "active", Encode.bool True ) ], index = 0, basePath = "/items/0" })
                    """{"$item": "active"}"""
                    |> Expect.equal (Ok True)
        , test "$item falsy check" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.object [ ( "active", Encode.bool False ) ], index = 0, basePath = "/items/0" })
                    """{"$item": "active"}"""
                    |> Expect.equal (Ok False)
        , test "$item equality check" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.object [ ( "status", Encode.string "done" ) ], index = 0, basePath = "/items/0" })
                    """{"$item": "status", "eq": "done"}"""
                    |> Expect.equal (Ok True)
        , test "$item equality check fails" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.object [ ( "status", Encode.string "pending" ) ], index = 0, basePath = "/items/0" })
                    """{"$item": "status", "eq": "done"}"""
                    |> Expect.equal (Ok False)
        , test "$item root reference" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.string "hello", index = 0, basePath = "/items/0" })
                    """{"$item": "", "eq": "hello"}"""
                    |> Expect.equal (Ok True)
        , test "$item with not" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.object [ ( "active", Encode.bool True ) ], index = 0, basePath = "/items/0" })
                    """{"$item": "active", "not": true}"""
                    |> Expect.equal (Ok False)
        , test "$item returns false when no repeat scope" <|
            \_ ->
                eval (Encode.object []) Nothing
                    """{"$item": "x"}"""
                    |> Expect.equal (Ok False)
        ]


indexSuite : Test
indexSuite =
    describe "$index conditions"
        [ test "$index equality check" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.null, index = 0, basePath = "/items/0" })
                    """{"$index": true, "eq": 0}"""
                    |> Expect.equal (Ok True)
        , test "$index equality check fails" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.null, index = 1, basePath = "/items/1" })
                    """{"$index": true, "eq": 0}"""
                    |> Expect.equal (Ok False)
        , test "$index gt check" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.null, index = 5, basePath = "/items/5" })
                    """{"$index": true, "gt": 2}"""
                    |> Expect.equal (Ok True)
        , test "$index truthiness" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.null, index = 3, basePath = "/items/3" })
                    """{"$index": true}"""
                    |> Expect.equal (Ok True)
        , test "$index zero is falsy" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.null, index = 0, basePath = "/items/0" })
                    """{"$index": true}"""
                    |> Expect.equal (Ok False)
        , test "$index with not" <|
            \_ ->
                eval (Encode.object [])
                    (Just { item = Encode.null, index = 1, basePath = "/items/1" })
                    """{"$index": true, "eq": 0, "not": true}"""
                    |> Expect.equal (Ok True)
        ]
