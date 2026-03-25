module SchemaParserTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import JsonRender.Internal.SchemaParser as SchemaParser exposing (ActionSchema, CatalogSchema, ComponentSchema, FieldType(..))
import Test exposing (..)


sampleSchema : String
sampleSchema =
    """
    {
      "components": {
        "Card": {
          "props": {
            "type": "object",
            "properties": {
              "title": { "type": "string" },
              "subtitle": { "type": "string" }
            },
            "required": ["title"]
          },
          "description": "A card container",
          "slots": ["default"]
        },
        "Button": {
          "props": {
            "type": "object",
            "properties": {
              "label": { "type": "string" },
              "disabled": { "type": "boolean" }
            },
            "required": ["label"]
          },
          "description": "A clickable button",
          "slots": []
        }
      },
      "actions": {
        "press": {
          "params": {
            "type": "object",
            "properties": {},
            "required": []
          },
          "description": "Generic button press"
        },
        "export": {
          "params": {
            "type": "object",
            "properties": {
              "format": { "type": "string" }
            },
            "required": ["format"]
          },
          "description": "Export data"
        }
      }
    }
    """


suite : Test
suite =
    describe "SchemaParser"
        [ test "parses component names" <|
            \_ ->
                case Decode.decodeString SchemaParser.decoder sampleSchema of
                    Ok catalog ->
                        Expect.equal
                            [ "Button", "Card" ]
                            (Dict.keys catalog.components |> List.sort)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "parses required and optional fields" <|
            \_ ->
                case Decode.decodeString SchemaParser.decoder sampleSchema of
                    Ok catalog ->
                        case Dict.get "Card" catalog.components of
                            Just comp ->
                                Expect.all
                                    [ \c ->
                                        Dict.get "title" c.fields
                                            |> Maybe.map .required
                                            |> Expect.equal (Just True)
                                    , \c ->
                                        Dict.get "subtitle" c.fields
                                            |> Maybe.map .required
                                            |> Expect.equal (Just False)
                                    ]
                                    comp

                            Nothing ->
                                Expect.fail "Card not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "parses field types" <|
            \_ ->
                case Decode.decodeString SchemaParser.decoder sampleSchema of
                    Ok catalog ->
                        case Dict.get "Button" catalog.components of
                            Just comp ->
                                Expect.all
                                    [ \c ->
                                        Dict.get "label" c.fields
                                            |> Maybe.map .fieldType
                                            |> Expect.equal (Just FString)
                                    , \c ->
                                        Dict.get "disabled" c.fields
                                            |> Maybe.map .fieldType
                                            |> Expect.equal (Just FBool)
                                    ]
                                    comp

                            Nothing ->
                                Expect.fail "Button not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "parses action names" <|
            \_ ->
                case Decode.decodeString SchemaParser.decoder sampleSchema of
                    Ok catalog ->
                        Expect.equal
                            [ "export", "press" ]
                            (Dict.keys catalog.actions |> List.sort)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "parses action params" <|
            \_ ->
                case Decode.decodeString SchemaParser.decoder sampleSchema of
                    Ok catalog ->
                        case Dict.get "export" catalog.actions of
                            Just action ->
                                Dict.get "format" action.params
                                    |> Maybe.map .fieldType
                                    |> Expect.equal (Just FString)

                            Nothing ->
                                Expect.fail "export action not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "parses action with empty params" <|
            \_ ->
                case Decode.decodeString SchemaParser.decoder sampleSchema of
                    Ok catalog ->
                        case Dict.get "press" catalog.actions of
                            Just action ->
                                Dict.size action.params
                                    |> Expect.equal 0

                            Nothing ->
                                Expect.fail "press action not found"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "parses catalog without actions key" <|
            \_ ->
                let
                    noActionsSchema =
                        """{"components":{"Card":{"props":{"type":"object","properties":{"title":{"type":"string"}},"required":["title"]},"description":"A card","slots":["default"]}}}"""
                in
                case Decode.decodeString SchemaParser.decoder noActionsSchema of
                    Ok catalog ->
                        Dict.size catalog.actions
                            |> Expect.equal 0

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]
