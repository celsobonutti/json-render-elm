module SpecTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Spec as Spec exposing (EventHandler(..))
import Test exposing (..)


specJson : String
specJson =
    """
    {
      "root": "card-1",
      "elements": {
        "card-1": {
          "type": "Card",
          "props": { "title": "Hello" },
          "children": ["btn-1"]
        },
        "btn-1": {
          "type": "Button",
          "props": { "label": "Click" },
          "children": []
        }
      }
    }
    """


suite : Test
suite =
    describe "JsonRender.Spec"
        [ test "decodes a basic spec" <|
            \_ ->
                case Decode.decodeString Spec.decoder specJson of
                    Ok spec ->
                        Expect.all
                            [ \s -> Expect.equal "card-1" s.root
                            , \s -> Expect.equal 2 (Dict.size s.elements)
                            ]
                            spec

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes element props as PropValues" <|
            \_ ->
                case Decode.decodeString Spec.decoder specJson of
                    Ok spec ->
                        case Dict.get "card-1" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just (StringValue "Hello"))
                                    (Dict.get "title" el.props)

                            Nothing ->
                                Expect.fail "element card-1 not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes $state expression" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "t",
                          "elements": {
                            "t": {
                              "type": "Text",
                              "props": { "value": { "$state": "/user/name" } },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "t" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just (StateExpr "/user/name"))
                                    (Dict.get "value" el.props)

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes $template expression" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "t",
                          "elements": {
                            "t": {
                              "type": "Text",
                              "props": { "greeting": { "$template": "Hello ${/name}" } },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "t" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just (TemplateExpr "Hello ${/name}"))
                                    (Dict.get "greeting" el.props)

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes $item expression" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "t",
                          "elements": {
                            "t": {
                              "type": "Text",
                              "props": { "name": { "$item": "name" } },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "t" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just (ItemExpr "name"))
                                    (Dict.get "name" el.props)

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes $index expression" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "t",
                          "elements": {
                            "t": {
                              "type": "Text",
                              "props": { "idx": { "$index": true } },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "t" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just IndexExpr)
                                    (Dict.get "idx" el.props)

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes numeric prop values" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "t",
                          "elements": {
                            "t": {
                              "type": "Metric",
                              "props": { "value": 42, "ratio": 3.14, "active": true },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "t" spec.elements of
                            Just el ->
                                Expect.all
                                    [ \e -> Expect.equal (Just (IntValue 42)) (Dict.get "value" e.props)
                                    , \e -> Expect.equal (Just (FloatValue 3.14)) (Dict.get "ratio" e.props)
                                    , \e -> Expect.equal (Just (BoolValue True)) (Dict.get "active" e.props)
                                    ]
                                    el

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes $bindState expression" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "t",
                          "elements": {
                            "t": {
                              "type": "Input",
                              "props": { "value": { "$bindState": "/form/name" } },
                              "children": []
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        case Dict.get "t" spec.elements of
                            Just el ->
                                Expect.equal
                                    (Just (BindStateExpr "/form/name"))
                                    (Dict.get "value" el.props)

                            Nothing ->
                                Expect.fail "element not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes a complex multi-element spec" <|
            \_ ->
                let
                    json =
                        """
                        {
                          "root": "form",
                          "elements": {
                            "form": {
                              "type": "Card",
                              "props": { "title": "Contact" },
                              "children": ["name-input", "greeting"]
                            },
                            "name-input": {
                              "type": "Input",
                              "props": {
                                "value": { "$bindState": "/form/name" },
                                "placeholder": "Your name"
                              },
                              "children": []
                            },
                            "greeting": {
                              "type": "Text",
                              "props": { "content": { "$template": "Hello ${/form/name}!" } },
                              "children": [],
                              "visible": { "$state": "/showGreeting" }
                            }
                          }
                        }
                        """
                in
                case Decode.decodeString Spec.decoder json of
                    Ok spec ->
                        Expect.all
                            [ \s -> Expect.equal "form" s.root
                            , \s -> Expect.equal 3 (Dict.size s.elements)
                            , \s ->
                                case Dict.get "name-input" s.elements of
                                    Just el ->
                                        Expect.equal (Just (BindStateExpr "/form/name")) (Dict.get "value" el.props)

                                    Nothing ->
                                        Expect.fail "name-input not found"
                            , \s ->
                                case Dict.get "greeting" s.elements of
                                    Just el ->
                                        Expect.all
                                            [ \e -> Expect.equal (Just (TemplateExpr "Hello ${/form/name}!")) (Dict.get "content" e.props)
                                            , \e -> Expect.notEqual Nothing e.visible
                                            ]
                                            el

                                    Nothing ->
                                        Expect.fail "greeting not found"
                            ]
                            spec

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , describe "on field"
            [ test "decodes element with on field containing single action" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "btn",
                              "elements": {
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": "Click" },
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
                            case Dict.get "btn" spec.elements of
                                Just el ->
                                    case Dict.get "press" el.on of
                                        Just (SingleAction binding) ->
                                            Expect.all
                                                [ \b -> Expect.equal "setState" b.action
                                                , \b -> Expect.equal (Just (StringValue "/clicked")) (Dict.get "path" b.params)
                                                , \b -> Expect.equal (Just (BoolValue True)) (Dict.get "value" b.params)
                                                ]
                                                binding

                                        Just (ChainedActions _) ->
                                            Expect.fail "expected SingleAction, got ChainedActions"

                                        Nothing ->
                                            Expect.fail "press handler not found"

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes element with on field containing chained actions array" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "btn",
                              "elements": {
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": "Add" },
                                  "children": [],
                                  "on": {
                                    "press": [
                                      { "action": "setState", "params": { "path": "/loading", "value": true } },
                                      { "action": "pushState", "params": { "path": "/items", "value": "new" } }
                                    ]
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "btn" spec.elements of
                                Just el ->
                                    case Dict.get "press" el.on of
                                        Just (ChainedActions bindings) ->
                                            Expect.all
                                                [ \bs -> Expect.equal 2 (List.length bs)
                                                , \bs ->
                                                    case List.head bs of
                                                        Just first ->
                                                            Expect.equal "setState" first.action

                                                        Nothing ->
                                                            Expect.fail "empty chain"
                                                , \bs ->
                                                    case List.drop 1 bs |> List.head of
                                                        Just second ->
                                                            Expect.equal "pushState" second.action

                                                        Nothing ->
                                                            Expect.fail "missing second action"
                                                ]
                                                bindings

                                        Just (SingleAction _) ->
                                            Expect.fail "expected ChainedActions, got SingleAction"

                                        Nothing ->
                                            Expect.fail "press handler not found"

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes element with on containing expression params" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "btn",
                              "elements": {
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": "Push" },
                                  "children": [],
                                  "on": {
                                    "press": {
                                      "action": "pushState",
                                      "params": {
                                        "path": "/items",
                                        "value": { "$state": "/newItem" }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "btn" spec.elements of
                                Just el ->
                                    case Dict.get "press" el.on of
                                        Just (SingleAction binding) ->
                                            Expect.all
                                                [ \b -> Expect.equal "pushState" b.action
                                                , \b -> Expect.equal (Just (StringValue "/items")) (Dict.get "path" b.params)
                                                , \b -> Expect.equal (Just (StateExpr "/newItem")) (Dict.get "value" b.params)
                                                ]
                                                binding

                                        _ ->
                                            Expect.fail "expected SingleAction with expression param"

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes element without on field as empty dict" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "Hello" },
                                  "children": []
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "t" spec.elements of
                                Just el ->
                                    Expect.equal Dict.empty el.on

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes on field with multiple event handlers" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "inp",
                              "elements": {
                                "inp": {
                                  "type": "Input",
                                  "props": { "value": "test" },
                                  "children": [],
                                  "on": {
                                    "change": { "action": "setState", "params": { "path": "/val", "value": "x" } },
                                    "submit": { "action": "setState", "params": { "path": "/submitted", "value": true } }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "inp" spec.elements of
                                Just el ->
                                    Expect.all
                                        [ \e -> Expect.equal 2 (Dict.size e.on)
                                        , \e -> Expect.notEqual Nothing (Dict.get "change" e.on)
                                        , \e -> Expect.notEqual Nothing (Dict.get "submit" e.on)
                                        ]
                                        el

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes on field with $item expression in params" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "btn",
                              "elements": {
                                "btn": {
                                  "type": "Button",
                                  "props": { "label": "Remove" },
                                  "children": [],
                                  "on": {
                                    "press": {
                                      "action": "removeState",
                                      "params": {
                                        "path": { "$template": "/todos/${/currentIndex}" }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "btn" spec.elements of
                                Just el ->
                                    case Dict.get "press" el.on of
                                        Just (SingleAction binding) ->
                                            Expect.equal
                                                (Just (TemplateExpr "/todos/${/currentIndex}"))
                                                (Dict.get "path" binding.params)

                                        _ ->
                                            Expect.fail "expected SingleAction"

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        , describe "$computed decoding"
            [ test "decodes $computed expression in props" <|
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
                                      "$computed": "formatCurrency",
                                      "args": {
                                        "amount": { "$state": "/price" }
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
                            case Dict.get "t" spec.elements of
                                Just el ->
                                    Expect.equal
                                        (Just
                                            (ComputedExpr "formatCurrency"
                                                (Dict.fromList [ ( "amount", StateExpr "/price" ) ])
                                            )
                                        )
                                        (Dict.get "content" el.props)

                                Nothing ->
                                    Expect.fail "element t not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes $computed with no args" <|
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
                                    "content": { "$computed": "getTimestamp" }
                                  },
                                  "children": []
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "t" spec.elements of
                                Just el ->
                                    Expect.equal
                                        (Just (ComputedExpr "getTimestamp" Dict.empty))
                                        (Dict.get "content" el.props)

                                Nothing ->
                                    Expect.fail "element t not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes nested $computed in args" <|
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
                                      "$computed": "format",
                                      "args": {
                                        "value": {
                                          "$computed": "add",
                                          "args": { "a": 1, "b": 2 }
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
                            case Dict.get "t" spec.elements of
                                Just el ->
                                    case Dict.get "content" el.props of
                                        Just (ComputedExpr "format" args) ->
                                            case Dict.get "value" args of
                                                Just (ComputedExpr "add" _) ->
                                                    Expect.pass

                                                _ ->
                                                    Expect.fail "nested $computed not decoded"

                                        _ ->
                                            Expect.fail "outer $computed not decoded"

                                Nothing ->
                                    Expect.fail "element t not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        , describe "watch field"
            [ test "decodes element with watch containing single action" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "sel",
                              "elements": {
                                "sel": {
                                  "type": "Select",
                                  "props": { "label": "Country" },
                                  "children": [],
                                  "watch": {
                                    "/form/country": {
                                      "action": "setState",
                                      "params": { "path": "/form/city", "value": "" }
                                    }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "sel" spec.elements of
                                Just el ->
                                    case Dict.get "/form/country" el.watch of
                                        Just (SingleAction binding) ->
                                            Expect.equal "setState" binding.action

                                        Just (ChainedActions _) ->
                                            Expect.fail "expected SingleAction, got ChainedActions"

                                        Nothing ->
                                            Expect.fail "watcher not found"

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes element with watch containing chained actions" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "sel",
                              "elements": {
                                "sel": {
                                  "type": "Select",
                                  "props": { "label": "Country" },
                                  "children": [],
                                  "watch": {
                                    "/form/country": [
                                      { "action": "loadCities", "params": { "country": { "$state": "/form/country" } } },
                                      { "action": "setState", "params": { "path": "/form/city", "value": "" } }
                                    ]
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "sel" spec.elements of
                                Just el ->
                                    case Dict.get "/form/country" el.watch of
                                        Just (ChainedActions bindings) ->
                                            Expect.equal 2 (List.length bindings)

                                        _ ->
                                            Expect.fail "expected ChainedActions"

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes element without watch as empty dict" <|
                \_ ->
                    case Decode.decodeString Spec.decoder specJson of
                        Ok spec ->
                            case Dict.get "card-1" spec.elements of
                                Just el ->
                                    Expect.equal Dict.empty el.watch

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            , test "decodes element with multiple watched paths" <|
                \_ ->
                    let
                        json =
                            """
                            {
                              "root": "t",
                              "elements": {
                                "t": {
                                  "type": "Text",
                                  "props": { "content": "ok" },
                                  "children": [],
                                  "watch": {
                                    "/a": { "action": "setState", "params": { "path": "/x", "value": 1 } },
                                    "/b": { "action": "setState", "params": { "path": "/y", "value": 2 } }
                                  }
                                }
                              }
                            }
                            """
                    in
                    case Decode.decodeString Spec.decoder json of
                        Ok spec ->
                            case Dict.get "t" spec.elements of
                                Just el ->
                                    Expect.equal 2 (Dict.size el.watch)

                                Nothing ->
                                    Expect.fail "element not found"

                        Err err ->
                            Expect.fail (Decode.errorToString err)
            ]
        ]
