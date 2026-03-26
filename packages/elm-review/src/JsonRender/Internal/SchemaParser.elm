module JsonRender.Internal.SchemaParser exposing
    ( ActionSchema
    , CatalogSchema
    , ComponentSchema
    , FieldSchema
    , FieldType(..)
    , FunctionSchema
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Set


type alias CatalogSchema =
    { components : Dict String ComponentSchema
    , actions : Dict String ActionSchema
    , functions : Dict String FunctionSchema
    }


type alias ActionSchema =
    { params : Dict String FieldSchema
    , description : String
    }


type alias FunctionSchema =
    { params : Dict String FieldSchema
    , returnType : FieldType
    , description : String
    }


type alias ComponentSchema =
    { fields : Dict String FieldSchema
    , description : String
    , slots : List String
    }


type alias FieldSchema =
    { fieldType : FieldType
    , required : Bool
    }


type FieldType
    = FString
    | FInt
    | FFloat
    | FBool
    | FNullable FieldType
    | FList FieldType
    | FObject (Dict String FieldSchema)
    | FEnum (List String)


decoder : Decoder CatalogSchema
decoder =
    Decode.succeed CatalogSchema
        |> required "components" (Decode.dict componentDecoder)
        |> optional "actions" (Decode.dict actionDecoder) Dict.empty
        |> optional "functions" (Decode.dict functionDecoder) Dict.empty


actionDecoder : Decoder ActionSchema
actionDecoder =
    Decode.succeed (\params description -> ActionSchema params description)
        |> required "params" propsObjectDecoder
        |> required "description" Decode.string


functionDecoder : Decoder FunctionSchema
functionDecoder =
    Decode.succeed FunctionSchema
        |> required "params" propsObjectDecoder
        |> required "returnType" fieldTypeDecoder
        |> required "description" Decode.string


componentDecoder : Decoder ComponentSchema
componentDecoder =
    Decode.succeed (\fields description slots -> ComponentSchema fields description slots)
        |> required "props" propsObjectDecoder
        |> required "description" Decode.string
        |> optional "slots" (Decode.list Decode.string) []


propsObjectDecoder : Decoder (Dict String FieldSchema)
propsObjectDecoder =
    Decode.succeed
        (\properties requiredFields ->
            Dict.map
                (\key fieldType ->
                    { fieldType = fieldType
                    , required = Set.member key requiredFields
                    }
                )
                properties
        )
        |> required "properties" (Decode.dict fieldTypeDecoder)
        |> optional "required" (Decode.list Decode.string |> Decode.map Set.fromList) Set.empty


fieldTypeDecoder : Decoder FieldType
fieldTypeDecoder =
    Decode.oneOf
        [ -- Enum
          Decode.field "enum" (Decode.list Decode.string) |> Decode.map FEnum

        -- Nullable
        , Decode.field "nullable" Decode.bool
            |> Decode.andThen
                (\isNullable ->
                    if isNullable then
                        Decode.field "type" simpleTypeDecoder |> Decode.map FNullable

                    else
                        Decode.field "type" simpleTypeDecoder
                )

        -- Array
        , Decode.field "type" Decode.string
            |> Decode.andThen
                (\t ->
                    if t == "array" then
                        Decode.field "items" (Decode.lazy (\_ -> fieldTypeDecoder))
                            |> Decode.map FList

                    else if t == "object" then
                        Decode.field "properties" (Decode.lazy (\_ -> propsObjectDecoder))
                            |> Decode.map FObject

                    else
                        simpleTypeFromString t
                            |> Maybe.map Decode.succeed
                            |> Maybe.withDefault (Decode.fail ("unknown type: " ++ t))
                )
        ]


simpleTypeDecoder : Decoder FieldType
simpleTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\t ->
                simpleTypeFromString t
                    |> Maybe.map Decode.succeed
                    |> Maybe.withDefault (Decode.fail ("unknown type: " ++ t))
            )


simpleTypeFromString : String -> Maybe FieldType
simpleTypeFromString t =
    case t of
        "string" ->
            Just FString

        "integer" ->
            Just FInt

        "number" ->
            Just FFloat

        "boolean" ->
            Just FBool

        _ ->
            Nothing
