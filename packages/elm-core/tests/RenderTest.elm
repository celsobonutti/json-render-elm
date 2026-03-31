module RenderTest exposing (..)

import Dict
import Expect
import Html exposing (div, text)
import Html.Attributes exposing (class)
import JsonRender.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Render as Render
import JsonRender.Resolve as Resolve
import JsonRender.Spec as Spec
import JsonRender.Visibility exposing (VisibilityCondition(..), Source(..), Operator(..))
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


testRegistry : Render.Registry msg
testRegistry =
    { components =
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
                            , Events.onClick (ctx.emit "press")
                            ]
                            [ text ctx.props ]
                    )
              )
            ]
    , functions =
        Dict.fromList
            [ ( "shout"
              , \args ->
                    case Dict.get "text" args of
                        Just (Resolve.RString s) ->
                            Resolve.RString (String.toUpper s)

                        _ ->
                            Resolve.RError "shout: expected string arg 'text'"
              )
            , ( "wrap"
              , \args ->
                    case Dict.get "text" args of
                        Just (Resolve.RString s) ->
                            Resolve.RString ("[" ++ s ++ "]")

                        _ ->
                            Resolve.RError "wrap: expected string arg 'text'"
              )
            ]
    }


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
                                    , watch = Dict.empty
                                    }
                                  )
                                ]
                        , state = Nothing
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
                                    , watch = Dict.empty
                                    }
                                  )
                                , ( "inner"
                                  , { type_ = "Text"
                                    , props = Dict.fromList [ ( "content", StringValue "Body" ) ]
                                    , children = []
                                    , visible = Nothing
                                    , repeat = Nothing
                                    , on = Dict.empty
                                    , watch = Dict.empty
                                    }
                                  )
                                ]
                        , state = Nothing
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
                                    , watch = Dict.empty
                                    }
                                  )
                                ]
                        , state = Nothing
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
                                    , visible = Just (Compare (StateSource "/show") IsTruthy)
                                    , repeat = Nothing
                                    , on = Dict.empty
                                    , watch = Dict.empty
                                    }
                                  )
                                ]
                        , state = Nothing
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
                        { components =
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
                        , functions = Dict.empty
                        }

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
                                    , watch = Dict.empty
                                    }
                                  )
                                ]
                        , state = Nothing
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
                                  "visible": { "$state": "/show" }
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
                                  "visible": { "$state": "/show" }
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
                                  "visible": { "$state": "/showGreeting" }
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
            , test "resolves $computed expression from JSON spec" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": {
                                    "content": {
                                      "$computed": "shout",
                                      "args": { "text": { "$state": "/greeting" } }
                                    }
                                  },
                                  "children": []
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "greeting", Encode.string "hello" ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "HELLO" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "unknown $computed shows error" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": {
                                    "content": { "$computed": "nonexistent", "args": {} }
                                  },
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
                                |> Query.has [ Selector.text "Props error" ]

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
                                      "params": { "statePath": "/clicked", "value": true }
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
                                    "press": { "action": "setState", "params": { "statePath": "/x", "value": 1 } }
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
                                      { "action": "pushState", "params": { "statePath": "/todos", "value": { "$state": "/newTodo" } } },
                                      { "action": "setState", "params": { "statePath": "/newTodo", "value": "" } }
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
                                    "press": { "action": "setState", "params": { "statePath": "/submitted", "value": true } }
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
        , describe "nested $computed expressions"
            [ test "$computed with $state arg" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": {
                                    "content": {
                                      "$computed": "shout",
                                      "args": { "text": { "$state": "/greeting" } }
                                    }
                                  },
                                  "children": []
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "greeting", Encode.string "hello" ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "HELLO" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "nested $computed (one as arg to another)" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": {
                                    "content": {
                                      "$computed": "wrap",
                                      "args": {
                                        "text": {
                                          "$computed": "shout",
                                          "args": { "text": "hello" }
                                        }
                                      }
                                    }
                                  },
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
                                |> Query.has [ Selector.text "[HELLO]" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "$computed inside $cond then branch" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": {
                                    "content": {
                                      "$cond": { "$state": "/loud" },
                                      "$then": {
                                        "$computed": "shout",
                                        "args": { "text": "hello" }
                                      },
                                      "$else": "hello"
                                    }
                                  },
                                  "children": []
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "loud", Encode.bool True ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "HELLO" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "$computed as $cond condition (truthy result)" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": {
                                    "content": {
                                      "$cond": {
                                        "$computed": "shout",
                                        "args": { "text": "yes" }
                                      },
                                      "$then": "truthy",
                                      "$else": "falsy"
                                    }
                                  },
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
                                |> Query.has [ Selector.text "truthy" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "$computed with $item arg inside repeat" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "card",
                              "elements": {
                                "card": {
                                  "type": "Card",
                                  "props": { "title": "Items" },
                                  "children": ["item"],
                                  "repeat": { "statePath": "/items", "key": "id" }
                                },
                                "item": {
                                  "type": "Text",
                                  "props": {
                                    "content": {
                                      "$computed": "shout",
                                      "args": { "text": { "$item": "name" } }
                                    }
                                  },
                                  "children": []
                                }
                              }
                            }
                            """

                        state =
                            Encode.object
                                [ ( "items"
                                  , Encode.list identity
                                        [ Encode.object [ ( "id", Encode.string "1" ), ( "name", Encode.string "alice" ) ]
                                        , Encode.object [ ( "id", Encode.string "2" ), ( "name", Encode.string "bob" ) ]
                                        ]
                                  )
                                ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render testRegistry state spec
                                |> Query.fromHtml
                                |> Query.has [ Selector.text "ALICE", Selector.text "BOB" ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        , describe "$computed with $bindState"
            [ test "$computed reads same state as $bindState input" <|
                \_ ->
                    let
                        bindComputedRegistry : Render.Registry msg
                        bindComputedRegistry =
                            { components =
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
                                                div [ class "input" ]
                                                    [ text ctx.props
                                                    , case ctx.bindings.value of
                                                        Just _ ->
                                                            text "[bound]"

                                                        Nothing ->
                                                            text "[unbound]"
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
                                            (\ctx -> div [ class "display" ] [ text ctx.props ])
                                      )
                                    , ( "Stack"
                                      , Render.register
                                            (\_ -> Ok ())
                                            (\_ -> ())
                                            (\ctx -> div [ class "stack" ] ctx.children)
                                      )
                                    ]
                            , functions =
                                Dict.fromList
                                    [ ( "shout"
                                      , \args ->
                                            case Dict.get "text" args of
                                                Just (Resolve.RString s) ->
                                                    Resolve.RString (String.toUpper s)

                                                _ ->
                                                    Resolve.RError "shout: expected string arg 'text'"
                                      )
                                    ]
                            }

                        json =
                            """
                            {
                              "root": "stack",
                              "elements": {
                                "stack": {
                                  "type": "Stack",
                                  "props": {},
                                  "children": ["input", "display"]
                                },
                                "input": {
                                  "type": "Input",
                                  "props": { "value": { "$bindState": "/name" } },
                                  "children": []
                                },
                                "display": {
                                  "type": "Text",
                                  "props": {
                                    "content": {
                                      "$computed": "shout",
                                      "args": { "text": { "$state": "/name" } }
                                    }
                                  },
                                  "children": []
                                }
                              }
                            }
                            """

                        state =
                            Encode.object [ ( "name", Encode.string "alice" ) ]
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            Render.render bindComputedRegistry state spec
                                |> Query.fromHtml
                                |> Expect.all
                                    [ Query.find [ Selector.class "input" ]
                                        >> Query.has [ Selector.text "alice", Selector.text "[bound]" ]
                                    , Query.find [ Selector.class "display" ]
                                        >> Query.has [ Selector.text "ALICE" ]
                                    ]

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        ]
