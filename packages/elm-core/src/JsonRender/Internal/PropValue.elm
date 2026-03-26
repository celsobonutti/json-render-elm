module JsonRender.Internal.PropValue exposing
    ( PropValue(..)
    , decoder
    )

{-| Shared PropValue type and decoder, used by both Spec and Visibility.
-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)


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
    | ConditionalExpr PropValue PropValue PropValue
    | ComputedExpr String (Dict String PropValue)


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
        , Decode.map3 ConditionalExpr
            (Decode.field "$cond" (Decode.lazy (\_ -> decoder)))
            (Decode.field "$then" (Decode.lazy (\_ -> decoder)))
            (Decode.field "$else" (Decode.lazy (\_ -> decoder)))
        , Decode.field "$computed" Decode.string
            |> Decode.andThen
                (\name ->
                    Decode.oneOf
                        [ Decode.field "args" (Decode.lazy (\_ -> Decode.dict decoder))
                        , Decode.succeed Dict.empty
                        ]
                        |> Decode.map (ComputedExpr name)
                )

        -- Literals
        , Decode.null NullValue
        , Decode.bool |> Decode.map BoolValue
        , Decode.int |> Decode.map IntValue
        , Decode.float |> Decode.map FloatValue
        , Decode.string |> Decode.map StringValue
        , Decode.lazy (\_ -> Decode.list decoder) |> Decode.map ListValue
        , Decode.lazy (\_ -> Decode.dict decoder) |> Decode.map ObjectValue
        ]
