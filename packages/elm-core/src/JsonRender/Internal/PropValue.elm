module JsonRender.Internal.PropValue exposing
    ( ConditionExpr(..)
    , PropValue(..)
    , decoder
    , encode
    )

{-| Shared PropValue type and decoder, used by both Spec and Visibility.
-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import JsonRender.Internal.Condition as Condition


type PropValue
    = StringValue String
    | IntValue Int
    | FloatValue Float
    | BoolValue Bool
    | NullValue
    | ListValue (List PropValue)
    | ObjectValue (Dict String PropValue)
    | StateExpr String
    | ItemExpr String
    | IndexExpr
    | TemplateExpr String
    | BindStateExpr String
    | BindItemExpr String
    | ConditionalExpr ConditionExpr PropValue PropValue
    | ComputedExpr String (Dict String PropValue)


type ConditionExpr
    = VisibilityCondition Condition.Condition
    | TruthyExpr PropValue


decoder : Decoder PropValue
decoder =
    Decode.oneOf
        [ -- Expressions first (objects with special keys)
          Decode.field "$state" Decode.string |> Decode.map StateExpr
        , Decode.field "$item" Decode.string |> Decode.map ItemExpr
        , Decode.field "$index" (Decode.succeed IndexExpr)
        , Decode.field "$template" Decode.string |> Decode.map TemplateExpr
        , Decode.field "$bindState" Decode.string |> Decode.map BindStateExpr
        , Decode.field "$bindItem" Decode.string |> Decode.map BindItemExpr
        , Decode.succeed ConditionalExpr
            |> required "$cond" conditionExprDecoder
            |> required "$then" (Decode.lazy (\_ -> decoder))
            |> required "$else" (Decode.lazy (\_ -> decoder))
        , Decode.succeed (\name args -> ComputedExpr name args)
            |> required "$computed" Decode.string
            |> optional "args" (Decode.lazy (\_ -> Decode.dict decoder)) Dict.empty

        -- Literals
        , Decode.null NullValue
        , Decode.bool |> Decode.map BoolValue
        , Decode.int |> Decode.map IntValue
        , Decode.float |> Decode.map FloatValue
        , Decode.string |> Decode.map StringValue
        , Decode.lazy (\_ -> Decode.list decoder) |> Decode.map ListValue
        , Decode.lazy (\_ -> Decode.dict decoder) |> Decode.map ObjectValue
        ]


encode : PropValue -> Encode.Value
encode pv =
    encodeHelp pv


encodeHelp : PropValue -> Encode.Value
encodeHelp pv =
    case pv of
        StringValue s ->
            Encode.string s

        IntValue n ->
            Encode.int n

        FloatValue f ->
            Encode.float f

        BoolValue b ->
            Encode.bool b

        NullValue ->
            Encode.null

        StateExpr path ->
            Encode.object [ ( "$state", Encode.string path ) ]

        BindStateExpr path ->
            Encode.object [ ( "$bindState", Encode.string path ) ]

        ItemExpr field ->
            Encode.object [ ( "$item", Encode.string field ) ]

        BindItemExpr field ->
            Encode.object [ ( "$bindItem", Encode.string field ) ]

        IndexExpr ->
            Encode.object [ ( "$index", Encode.bool True ) ]

        TemplateExpr t ->
            Encode.object [ ( "$template", Encode.string t ) ]

        ConditionalExpr _ _ _ ->
            Encode.string "<cond>"

        ComputedExpr _ _ ->
            Encode.string "<computed>"

        ListValue items ->
            items |> List.map encode |> Encode.list identity

        ObjectValue fields ->
            fields |> Dict.map (\_ v -> encode v) |> Dict.toList |> Encode.object


conditionExprDecoder : Decoder ConditionExpr
conditionExprDecoder =
    Decode.oneOf
        [ Condition.decoder |> Decode.map VisibilityCondition
        , Decode.lazy (\_ -> decoder) |> Decode.map TruthyExpr
        ]
