module RenderTest exposing (..)

import Dict
import Expect
import Html exposing (div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Render as Render
import JsonRender.Resolve as Resolve
import JsonRender.Spec as Spec
import JsonRender.Visibility exposing (VisibilityCondition(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


testRegistry : Render.Registry msg
testRegistry =
    Dict.fromList
        [ ( "Card"
          , Render.register
                (\props ->
                    Resolve.succeed identity
                        |> Resolve.required "title" Resolve.string
                        |> (\d -> d props)
                )
                (\_ -> ())
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
                (\_ -> ())
                (\ctx -> text ctx.props)
          )
        , ( "Button"
          , Render.register
                (\props ->
                    Resolve.succeed identity
                        |> Resolve.required "label" Resolve.string
                        |> (\d -> d props)
                )
                (\_ -> ())
                (\ctx ->
                    Html.button
                        [ class "button"
                        , onClick (ctx.emit "press")
                        ]
                        [ text ctx.props ]
                )
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
                                    , repeat = Nothing
                                    , on = Dict.empty
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
                                    , repeat = Nothing
                                    , on = Dict.empty
                                    }
                                  )
                                , ( "inner"
                                  , { type_ = "Text"
                                    , props = Dict.fromList [ ( "content", StringValue "Body" ) ]
                                    , children = []
                                    , visible = Nothing
                                    , repeat = Nothing
                                    , on = Dict.empty
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
                                    , repeat = Nothing
                                    , on = Dict.empty
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
                                    , repeat = Nothing
                                    , on = Dict.empty
                                    }
                                  )
                                ]
                        }
                in
                Render.render testRegistry state spec
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.text "Hidden" ]
        , test "resolves $bindState and provides setter binding" <|
            \_ ->
                let
                    bindRegistry : Render.Registry msg
                    bindRegistry =
                        Dict.fromList
                            [ ( "Input"
                              , Render.register
                                    (\props ->
                                        Resolve.succeed identity
                                            |> Resolve.required "value" Resolve.string
                                            |> (\d -> d props)
                                    )
                                    (\bindings ->
                                        { value = Dict.get "value" bindings }
                                    )
                                    (\ctx ->
                                        div []
                                            [ text ctx.props
                                            , case ctx.bindings.value of
                                                Just _ ->
                                                    text "[bound]"

                                                Nothing ->
                                                    text "[unbound]"
                                            ]
                                    )
                              )
                            ]

                    formState =
                        Encode.object
                            [ ( "form"
                              , Encode.object [ ( "name", Encode.string "Alice" ) ]
                              )
                            ]

                    spec =
                        { root = "input-1"
                        , elements =
                            Dict.fromList
                                [ ( "input-1"
                                  , { type_ = "Input"
                                    , props = Dict.fromList [ ( "value", BindStateExpr "/form/name" ) ]
                                    , children = []
                                    , visible = Nothing
                                    , repeat = Nothing
                                    , on = Dict.empty
                                    }
                                  )
                                ]
                        }
                in
                Render.render bindRegistry formState spec
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Alice", Selector.text "[bound]" ]
        , describe "full pipeline (JSON → decode → render)"
            [ test "renders a card from JSON spec" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "card-1",
                              "elements": {
                                "card-1": {
                                  "type": "Card",
                                  "props": { "title": "Hello World" },
                                  "children": []
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry Encode.null spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.class "card", Selector.text "Hello World" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "renders nested children from JSON spec" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "card-1",
                              "elements": {
                                "card-1": {
                                  "type": "Card",
                                  "props": { "title": "Parent" },
                                  "children": ["text-1"]
                                },
                                "text-1": {
                                  "type": "Text",
                                  "props": { "content": "Child content" },
                                  "children": []
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry Encode.null spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Parent", Selector.text "Child content" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "resolves $state expression from JSON spec" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": { "$state": "/greeting" } },
                                  "children": []
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "greeting", Encode.string "Hello Alice" ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Hello Alice" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "resolves $template expression from JSON spec" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": { "$template": "Welcome, ${/user/name}!" } },
                                  "children": []
                                }
                              }
                            }
                            """

                        state =
                            Encode.object
                                [ ( "user", Encode.object [ ( "name", Encode.string "Bob" ) ] ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Welcome, Bob!" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "visibility hides element from JSON spec" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "Secret" },
                                  "children": [],
                                  "visible": { "truthy": "/show" }
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "show", Encode.bool False ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.hasNot [ Selector.text "Secret" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "visibility shows element from JSON spec" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "Visible" },
                                  "children": [],
                                  "visible": { "truthy": "/show" }
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "show", Encode.bool True ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Visible" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "full contact form pipeline from JSON" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "form",
                              "elements": {
                                "form": {
                                  "type": "Card",
                                  "props": { "title": "Contact Us" },
                                  "children": ["greeting"]
                                },
                                "greeting": {
                                  "type": "Text",
                                  "props": { "content": { "$template": "Hello, ${/user/name}!" } },
                                  "children": [],
                                  "visible": { "truthy": "/showGreeting" }
                                }
                              }
                            }
                            """

                        state =
                            Encode.object
                                [ ( "user", Encode.object [ ( "name", Encode.string "Alice" ) ] )
                                , ( "showGreeting", Encode.bool True )
                                ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has
                                    [ Selector.text "Contact Us"
                                    , Selector.text "Hello, Alice!"
                                    ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        , describe "emit and on"
            [ test "button with on.press renders and has button element" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "btn",
                              "elements": {
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": "Click Me" },
                                  "children": [],
                                  "on": {
                                    "press": {
                                      "action": "setState",
                                      "params": { "path": "/clicked", "value": true }
                                    }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry Encode.null spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.class "button", Selector.text "Click Me" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "element without on field still renders normally" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "No events" },
                                  "children": []
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry Encode.null spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "No events" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "on field does not affect props resolution" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "btn",
                              "elements": {
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": { "$state": "/buttonLabel" } },
                                  "children": [],
                                  "on": {
                                    "press": { "action": "setState", "params": { "path": "/x", "value": 1 } }
                                  }
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "buttonLabel", Encode.string "Dynamic Label" ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "Dynamic Label" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "button with chained on.press renders correctly" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "btn",
                              "elements": {
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": "Add Todo" },
                                  "children": [],
                                  "on": {
                                    "press": [
                                      { "action": "pushState", "params": { "path": "/todos", "value": { "$state": "/newTodo" } } },
                                      { "action": "setState", "params": { "path": "/newTodo", "value": "" } }
                                    ]
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry Encode.null spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.class "button", Selector.text "Add Todo" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "button with on.press alongside siblings renders full tree" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "card",
                              "elements": {
                                "card": {
                                  "type": "Card",
                                  "props": { "title": "Form" },
                                  "children": ["msg", "btn"]
                                },
                                "msg": {
                                  "type": "Text",
                                  "props": { "content": "Fill in the form" },
                                  "children": []
                                },
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": "Submit" },
                                  "children": [],
                                  "on": {
                                    "press": { "action": "setState", "params": { "path": "/submitted", "value": true } }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry Encode.null spec
                                |> Query.fromHtml
                                |> Query.has
                                    [ Selector.text "Form"
                                    , Selector.text "Fill in the form"
                                    , Selector.text "Submit"
                                    ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        ]
