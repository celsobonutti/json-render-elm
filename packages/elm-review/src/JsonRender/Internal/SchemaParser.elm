module JsonRender.Internal.SchemaParser exposing
    ( CatalogSchema
    , ComponentSchema
    , FieldSchema
    , FieldType(..)
    , decoder
    )

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Set exposing (Set)


type alias CatalogSchema =
    { components : Dict String ComponentSchema
    }


type alias ComponentSchema =
    { fields : Dict String FieldSchema
    , description : String
    , hasChildren : Bool
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


componentDecoder : Decoder ComponentSchema
componentDecoder =
    Decode.field "props" propsObjectDecoder
        |> Decode.andThen
            (\fields ->
                Decode.succeed (ComponentSchema fields)
                    |> required "description" Decode.string
                    |> optional "hasChildren" Decode.bool False
            )


propsObjectDecoder : Decoder (Dict String FieldSchema)
propsObjectDecoder =
    Decode.map2
        (\properties requiredFields ->
            Dict.map
                (\key fieldType ->
                    { fieldType = fieldType
                    , required = Set.member key requiredFields
                    }
                )
                properties
        )
        (Decode.field "properties" (Decode.dict fieldTypeDecoder))
        (Decode.oneOf
            [ Decode.field "required" (Decode.list Decode.string |> Decode.map Set.fromList)
            , Decode.succeed Set.empty
            ]
        )


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
