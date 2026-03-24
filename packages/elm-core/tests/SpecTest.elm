module SpecTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Spec as Spec
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
                              "visible": { "truthy": "/showGreeting" }
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
        ]
