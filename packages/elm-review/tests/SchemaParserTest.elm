module SchemaParserTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import JsonRender.Internal.SchemaParser as SchemaParser exposing (CatalogSchema, ComponentSchema, FieldType(..))
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
        ]
