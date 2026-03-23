module RenderTest exposing (..)

import Dict
import Expect
import Html exposing (div, text)
import Html.Attributes exposing (class)
import Json.Encode as Encode
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Render as Render exposing (Component, Registry)
import JsonRender.Resolve as Resolve exposing (ResolvedValue(..))
import JsonRender.Spec exposing (Element, Spec)
import JsonRender.Visibility exposing (VisibilityCondition(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


testRegistry : Registry
testRegistry =
    Dict.fromList
        [ ( "Card"
          , Render.register
                (\props ->
                    Resolve.succeed identity
                        |> Resolve.required "title" Resolve.string
                        |> (\d -> d props)
                )
                (\ctx ->
                    div [ class "card" ]
                        [ text ctx.props
                        , div [] ctx.children
                        ]
                )
          )
        , ( "Text"
          , Render.register
                (\props ->
                    Resolve.succeed identity
                        |> Resolve.required "content" Resolve.string
                        |> (\d -> d props)
                )
                (\ctx -> text ctx.props)
          )
        ]


suite : Test
suite =
    describe "JsonRender.Render"
        [ test "renders a single element" <|
            \_ ->
                let
                    spec =
                        { root = "t"
                        , elements =
                            Dict.fromList
                                [ ( "t"
                                  , { type_ = "Text"
                                    , props = Dict.fromList [ ( "content", StringValue "Hello" ) ]
                                    , children = []
                                    , visible = Nothing
                                    }
                                  )
                                ]
                        }
                in
                Render.render testRegistry Encode.null spec
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Hello" ]
        , test "renders nested elements" <|
            \_ ->
                let
                    spec =
                        { root = "card"
                        , elements =
                            Dict.fromList
                                [ ( "card"
                                  , { type_ = "Card"
                                    , props = Dict.fromList [ ( "title", StringValue "My Card" ) ]
                                    , children = [ "inner" ]
                                    , visible = Nothing
                                    }
                                  )
                                , ( "inner"
                                  , { type_ = "Text"
                                    , props = Dict.fromList [ ( "content", StringValue "Body" ) ]
                                    , children = []
                                    , visible = Nothing
                                    }
                                  )
                                ]
                        }
                in
                Render.render testRegistry Encode.null spec
                    |> Query.fromHtml
                    |> Query.has [ Selector.class "card", Selector.text "My Card", Selector.text "Body" ]
        , test "renders nothing for unknown component" <|
            \_ ->
                let
                    spec =
                        { root = "t"
                        , elements =
                            Dict.fromList
                                [ ( "t"
                                  , { type_ = "Unknown"
                                    , props = Dict.empty
                                    , children = []
                                    , visible = Nothing
                                    }
                                  )
                                ]
                        }
                in
                Render.render testRegistry Encode.null spec
                    |> Query.fromHtml
                    |> Query.has []
        , test "respects visibility condition" <|
            \_ ->
                let
                    state =
                        Encode.object [ ( "show", Encode.bool False ) ]

                    spec =
                        { root = "t"
                        , elements =
                            Dict.fromList
                                [ ( "t"
                                  , { type_ = "Text"
                                    , props = Dict.fromList [ ( "content", StringValue "Hidden" ) ]
                                    , children = []
                                    , visible = Just (Truthy "/show")
                                    }
                                  )
                                ]
                        }
                in
                Render.render testRegistry state spec
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.text "Hidden" ]
        ]
