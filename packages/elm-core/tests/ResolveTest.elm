module ResolveTest exposing (..)

import Dict
import Expect
import Json.Encode as Encode
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Resolve as Resolve exposing (ResolvedValue(..))
import Test exposing (..)


state : Encode.Value
state =
    Encode.object
        [ ( "user"
          , Encode.object
                [ ( "name", Encode.string "Alice" )
                , ( "age", Encode.int 30 )
                ]
          )
        ]


suite : Test
suite =
    describe "JsonRender.Resolve"
        [ describe "resolveProps"
            [ test "passes through literal values" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "title", StringValue "Hello" ) ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.equal
                        (Just (RString "Hello"))
                        (Dict.get "title" resolved)
            , test "resolves $state expression" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "name", StateExpr "/user/name" ) ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.equal
                        (Just (RString "Alice"))
                        (Dict.get "name" resolved)
            , test "resolves $template expression" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "msg", TemplateExpr "Hi ${/user/name}!" ) ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.equal
                        (Just (RString "Hi Alice!"))
                        (Dict.get "msg" resolved)
            , test "resolves $item expression" <|
                \_ ->
                    let
                        item =
                            Encode.object [ ( "id", Encode.string "abc" ) ]

                        ctx =
                            Just { item = item, index = 2, basePath = "/items/2" }

                        props =
                            Dict.fromList [ ( "itemId", ItemExpr "id" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RString "abc"))
                        (Dict.get "itemId" resolved)
            , test "resolves $index expression" <|
                \_ ->
                    let
                        ctx =
                            Just { item = Encode.null, index = 5, basePath = "/items/5" }

                        props =
                            Dict.fromList [ ( "idx", IndexExpr ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RInt 5))
                        (Dict.get "idx" resolved)
            , test "resolves $bindState expression (read direction)" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "value", BindStateExpr "/user/name" ) ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.equal
                        (Just (RString "Alice"))
                        (Dict.get "value" resolved)
            , test "resolves $bindItem expression (read direction)" <|
                \_ ->
                    let
                        item =
                            Encode.object [ ( "title", Encode.string "Buy milk" ) ]

                        ctx =
                            Just { item = item, index = 0, basePath = "/todos/0" }

                        props =
                            Dict.fromList [ ( "label", BindItemExpr "title" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RString "Buy milk"))
                        (Dict.get "label" resolved)
            , test "resolves $bindItem with empty string to whole item" <|
                \_ ->
                    let
                        item =
                            Encode.string "hello"

                        ctx =
                            Just { item = item, index = 0, basePath = "/items/0" }

                        props =
                            Dict.fromList [ ( "value", BindItemExpr "" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RString "hello"))
                        (Dict.get "value" resolved)
            , test "resolves $bindItem outside repeat context to RNull" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "value", BindItemExpr "title" ) ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.equal
                        (Just RNull)
                        (Dict.get "value" resolved)
            , test "resolves $item with empty string to whole item" <|
                \_ ->
                    let
                        item =
                            Encode.object [ ( "name", Encode.string "Alice" ) ]

                        ctx =
                            Just { item = item, index = 0, basePath = "/items/0" }

                        props =
                            Dict.fromList [ ( "data", ItemExpr "" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RObject (Dict.fromList [ ( "name", RString "Alice" ) ])))
                        (Dict.get "data" resolved)
            , test "resolves mixed props (literals + expressions)" <|
                \_ ->
                    let
                        props =
                            Dict.fromList
                                [ ( "title", StringValue "Contact" )
                                , ( "name", StateExpr "/user/name" )
                                , ( "greeting", TemplateExpr "Hi ${/user/name}!" )
                                , ( "value", BindStateExpr "/user/name" )
                                , ( "active", BoolValue True )
                                ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.all
                        [ \r -> Expect.equal (Just (RString "Contact")) (Dict.get "title" r)
                        , \r -> Expect.equal (Just (RString "Alice")) (Dict.get "name" r)
                        , \r -> Expect.equal (Just (RString "Hi Alice!")) (Dict.get "greeting" r)
                        , \r -> Expect.equal (Just (RString "Alice")) (Dict.get "value" r)
                        , \r -> Expect.equal (Just (RBool True)) (Dict.get "active" r)
                        ]
                        resolved
            ]
        , describe "jsonValueToResolved handles complex types"
            [ test "$state resolves object to RObject" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "user", StateExpr "/user" ) ]

                        resolved =
                            Resolve.resolveProps state Nothing props
                    in
                    Expect.equal
                        (Just (RObject (Dict.fromList [ ( "name", RString "Alice" ), ( "age", RInt 30 ) ])))
                        (Dict.get "user" resolved)
            , test "$state resolves array to RList" <|
                \_ ->
                    let
                        arrayState =
                            Encode.object
                                [ ( "tags", Encode.list Encode.string [ "elm", "fp" ] ) ]

                        props =
                            Dict.fromList [ ( "tags", StateExpr "/tags" ) ]

                        resolved =
                            Resolve.resolveProps arrayState Nothing props
                    in
                    Expect.equal
                        (Just (RList [ RString "elm", RString "fp" ]))
                        (Dict.get "tags" resolved)
            , test "$state resolves nested object/array" <|
                \_ ->
                    let
                        nestedState =
                            Encode.object
                                [ ( "data"
                                  , Encode.object
                                        [ ( "items", Encode.list Encode.int [ 1, 2, 3 ] ) ]
                                  )
                                ]

                        props =
                            Dict.fromList [ ( "data", StateExpr "/data" ) ]

                        resolved =
                            Resolve.resolveProps nestedState Nothing props
                    in
                    Expect.equal
                        (Just (RObject (Dict.fromList [ ( "items", RList [ RInt 1, RInt 2, RInt 3 ] ) ])))
                        (Dict.get "data" resolved)
            , test "$item resolves object field that is an array" <|
                \_ ->
                    let
                        item =
                            Encode.object
                                [ ( "tags", Encode.list Encode.string [ "a", "b" ] ) ]

                        ctx =
                            Just { item = item, index = 0, basePath = "/items/0" }

                        props =
                            Dict.fromList [ ( "tags", ItemExpr "tags" ) ]

                        resolved =
                            Resolve.resolveProps state ctx props
                    in
                    Expect.equal
                        (Just (RList [ RString "a", RString "b" ]))
                        (Dict.get "tags" resolved)
            ]
        , describe "pipeline decoders"
            [ test "succeed + required decodes props" <|
                \_ ->
                    let
                        props =
                            Dict.fromList
                                [ ( "title", RString "Hello" )
                                , ( "count", RInt 5 )
                                ]

                        result =
                            Resolve.succeed Tuple.pair
                                |> Resolve.required "title" Resolve.string
                                |> Resolve.required "count" Resolve.int
                                |> (\d -> d props)
                    in
                    Expect.equal (Ok ( "Hello", 5 )) result
            , test "optional returns default when missing" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "title", RString "Hi" ) ]

                        result =
                            Resolve.succeed Tuple.pair
                                |> Resolve.required "title" Resolve.string
                                |> Resolve.optional "subtitle" Resolve.string Nothing
                                |> (\d -> d props)
                    in
                    Expect.equal (Ok ( "Hi", Nothing )) result
            , test "required fails on missing field" <|
                \_ ->
                    let
                        props =
                            Dict.empty

                        result =
                            Resolve.succeed identity
                                |> Resolve.required "title" Resolve.string
                                |> (\d -> d props)
                    in
                    Expect.err result
            ]
        , describe "RError propagation"
            [ test "pipeline required propagates RError" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "title", RError "test error" ) ]

                        result =
                            Resolve.succeed identity
                                |> Resolve.required "title" Resolve.string
                                |> (\d -> d props)
                    in
                    Expect.equal (Err "field 'title': test error") result
            , test "pipeline optional propagates RError" <|
                \_ ->
                    let
                        props =
                            Dict.fromList [ ( "title", RError "bad value" ) ]

                        result =
                            Resolve.succeed identity
                                |> Resolve.optional "title" Resolve.string Nothing
                                |> (\d -> d props)
                    in
                    Expect.equal (Err "bad value") result
            ]
        ]
