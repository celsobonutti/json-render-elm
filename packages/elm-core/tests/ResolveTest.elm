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
                            Just { item = item, index = 2 }

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
                            Just { item = Encode.null, index = 5 }

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
        ]
