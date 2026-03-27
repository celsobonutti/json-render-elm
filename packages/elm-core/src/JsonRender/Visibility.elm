module JsonRender.Visibility exposing
    ( CompareValue(..)
    , Operator(..)
    , RepeatContext
    , Source(..)
    , VisibilityCondition(..)
    , decoder
    , evaluate
    )

{-| Conditional visibility for spec elements.

JSON format (matches json-render core):

Sources:
  - `{ "$state": "/path" }` — read from state
  - `{ "$item": "field" }` — read from repeat item
  - `{ "$index": true }` — read repeat index

Operators (appended to source object):
  - (none) — truthy check
  - `"eq": <value>` — equals
  - `"neq": <value>` — not equals
  - `"gt": <number>` — greater than
  - `"gte": <number>` — greater than or equal
  - `"lt": <number>` — less than
  - `"lte": <number>` — less than or equal

CompareValue (right-hand side of operators):
  - literal value (number, string, bool, null)
  - `{ "$state": "/path" }` — dynamic state reference

Modifiers:
  - `"not": true` — invert any condition

Combinators:
  - `[cond, cond]` — implicit AND (list of conditions)
  - `{ "$and": [cond, cond] }` — explicit AND
  - `{ "$or": [cond, cond] }` — OR
  - `true` / `false` — always visible/hidden

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import JsonRender.Internal.PropValue as PropValue exposing (PropValue(..))
import JsonRender.State as State


type VisibilityCondition
    = Compare Source Operator
    | Not VisibilityCondition
    | And (List VisibilityCondition)
    | Or (List VisibilityCondition)


type Source
    = StateSource String
    | ItemSource String
    | IndexSource


type Operator
    = IsTruthy
    | Eq CompareValue
    | Neq CompareValue
    | Gt CompareValue
    | Gte CompareValue
    | Lt CompareValue
    | Lte CompareValue


type CompareValue
    = Literal PropValue
    | StateRef String


type alias RepeatContext =
    { item : Value
    , index : Int
    , basePath : String
    }


evaluate : Value -> Maybe RepeatContext -> VisibilityCondition -> Result String Bool
evaluate state repeatCtx condition =
    case condition of
        Compare source operator ->
            let
                maybeLhs =
                    resolveSource state repeatCtx source
            in
            applyOperator state operator maybeLhs

        Not inner ->
            evaluate state repeatCtx inner
                |> Result.map not

        And conditions ->
            List.foldl
                (\c acc ->
                    case acc of
                        Err err ->
                            Err err

                        Ok True ->
                            evaluate state repeatCtx c

                        Ok False ->
                            Ok False
                )
                (Ok True)
                conditions

        Or conditions ->
            List.foldl
                (\c acc ->
                    case acc of
                        Err err ->
                            Err err

                        Ok True ->
                            Ok True

                        Ok False ->
                            evaluate state repeatCtx c
                )
                (Ok False)
                conditions


resolveSource : Value -> Maybe RepeatContext -> Source -> Maybe Value
resolveSource state repeatCtx source =
    case source of
        StateSource path ->
            State.get path state

        ItemSource field_ ->
            case repeatCtx of
                Just ctx ->
                    if field_ == "" then
                        Just ctx.item

                    else
                        State.get ("/" ++ field_) ctx.item

                Nothing ->
                    Nothing

        IndexSource ->
            case repeatCtx of
                Just ctx ->
                    Just (Encode.int ctx.index)

                Nothing ->
                    Nothing


applyOperator : Value -> Operator -> Maybe Value -> Result String Bool
applyOperator state operator maybeLhs =
    case operator of
        IsTruthy ->
            Ok (maybeLhs |> Maybe.map isTruthy |> Maybe.withDefault False)

        Eq cv ->
            case maybeLhs of
                Nothing ->
                    Ok False

                Just lhs ->
                    resolveCompareValue state cv
                        |> Result.map (\rhs -> valuesEqual lhs rhs)

        Neq cv ->
            case maybeLhs of
                Nothing ->
                    Ok True

                Just lhs ->
                    resolveCompareValue state cv
                        |> Result.map (\rhs -> not (valuesEqual lhs rhs))

        Gt cv ->
            numericOp (>) state maybeLhs cv

        Gte cv ->
            numericOp (>=) state maybeLhs cv

        Lt cv ->
            numericOp (<) state maybeLhs cv

        Lte cv ->
            numericOp (<=) state maybeLhs cv


resolveCompareValue : Value -> CompareValue -> Result String Value
resolveCompareValue state cv =
    case cv of
        Literal pv ->
            Ok (propValueToJson pv)

        StateRef path ->
            case State.get path state of
                Just val ->
                    Ok val

                Nothing ->
                    Ok Encode.null


numericOp : (Float -> Float -> Bool) -> Value -> Maybe Value -> CompareValue -> Result String Bool
numericOp op state maybeLhs cv =
    case maybeLhs of
        Nothing ->
            Ok False

        Just lhs ->
            case resolveCompareValue state cv of
                Ok rhs ->
                    case ( toNumber lhs, toNumber rhs ) of
                        ( Just l, Just r ) ->
                            Ok (op l r)

                        _ ->
                            Ok False

                Err e ->
                    Err e


toNumber : Value -> Maybe Float
toNumber value =
    case Decode.decodeValue Decode.int value of
        Ok n ->
            Just (toFloat n)

        Err _ ->
            case Decode.decodeValue Decode.float value of
                Ok f ->
                    Just f

                Err _ ->
                    Nothing


isTruthy : Value -> Bool
isTruthy value =
    case Decode.decodeValue Decode.bool value of
        Ok b ->
            b

        Err _ ->
            case Decode.decodeValue Decode.string value of
                Ok s ->
                    s /= ""

                Err _ ->
                    case Decode.decodeValue Decode.int value of
                        Ok n ->
                            n /= 0

                        Err _ ->
                            case Decode.decodeValue Decode.float value of
                                Ok f ->
                                    f /= 0.0

                                Err _ ->
                                    case Decode.decodeValue (Decode.null False) value of
                                        Ok _ ->
                                            False

                                        Err _ ->
                                            case Decode.decodeValue (Decode.list Decode.value) value of
                                                Ok items ->
                                                    not (List.isEmpty items)

                                                Err _ ->
                                                    True


valuesEqual : Value -> Value -> Bool
valuesEqual a b =
    Encode.encode 0 a == Encode.encode 0 b


propValueToJson : PropValue -> Value
propValueToJson pv =
    case pv of
        StringValue s ->
            Encode.string s

        IntValue i ->
            Encode.int i

        FloatValue f ->
            Encode.float f

        BoolValue b ->
            Encode.bool b

        NullValue ->
            Encode.null

        _ ->
            Encode.null


decoder : Decoder VisibilityCondition
decoder =
    Decode.oneOf
        [ sourceConditionDecoder
        , Decode.field "$and" (Decode.lazy (\_ -> Decode.list decoder) |> Decode.map And)
        , Decode.field "$or" (Decode.lazy (\_ -> Decode.list decoder) |> Decode.map Or)
        , Decode.list (Decode.lazy (\_ -> decoder)) |> Decode.map And
        , Decode.bool
            |> Decode.map
                (\b ->
                    if b then
                        And []

                    else
                        Or []
                )
        ]


sourceConditionDecoder : Decoder VisibilityCondition
sourceConditionDecoder =
    Decode.oneOf
        [ Decode.field "$state" Decode.string
            |> Decode.andThen (\path -> operatorDecoder (StateSource path))
        , Decode.field "$item" Decode.string
            |> Decode.andThen (\field_ -> operatorDecoder (ItemSource field_))
        , Decode.field "$index" (Decode.succeed ())
            |> Decode.andThen (\_ -> operatorDecoder IndexSource)
        ]


operatorDecoder : Source -> Decoder VisibilityCondition
operatorDecoder source =
    Decode.oneOf
        [ Decode.field "eq" compareValueDecoder
            |> Decode.andThen (\v -> maybeNot (Compare source (Eq v)))
        , Decode.field "neq" compareValueDecoder
            |> Decode.andThen (\v -> maybeNot (Compare source (Neq v)))
        , Decode.field "gt" compareValueDecoder
            |> Decode.andThen (\v -> maybeNot (Compare source (Gt v)))
        , Decode.field "gte" compareValueDecoder
            |> Decode.andThen (\v -> maybeNot (Compare source (Gte v)))
        , Decode.field "lt" compareValueDecoder
            |> Decode.andThen (\v -> maybeNot (Compare source (Lt v)))
        , Decode.field "lte" compareValueDecoder
            |> Decode.andThen (\v -> maybeNot (Compare source (Lte v)))
        , maybeNot (Compare source IsTruthy)
        ]


compareValueDecoder : Decoder CompareValue
compareValueDecoder =
    Decode.oneOf
        [ Decode.field "$state" Decode.string |> Decode.map StateRef
        , PropValue.decoder |> Decode.map Literal
        ]


maybeNot : VisibilityCondition -> Decoder VisibilityCondition
maybeNot condition =
    Decode.oneOf
        [ Decode.field "not" Decode.bool
            |> Decode.andThen
                (\isNot ->
                    if isNot then
                        Decode.succeed (Not condition)

                    else
                        Decode.succeed condition
                )
        , Decode.succeed condition
        ]
