module JsonRender.Visibility exposing
    ( VisibilityCondition(..)
    , decoder
    , evaluate
    )

{-| Conditional visibility for spec elements.

JSON format (matches json-render core prompt):

  - `{ "$state": "/path" }` — truthy check
  - `{ "$state": "/path", "not": true }` — falsy check
  - `{ "$state": "/path", "eq": <value> }` — equals
  - `{ "$state": "/path", "neq": <value> }` — not equals
  - Any condition can add `"not": true` to invert
  - `[cond, cond]` — implicit AND (list of conditions)
  - `{ "$and": [cond, cond] }` — explicit AND
  - `{ "$or": [cond, cond] }` — OR
  - `true` / `false` — always visible/hidden

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode exposing (Value)
import JsonRender.Internal.PropValue as PropValue exposing (PropValue(..))
import JsonRender.State as State


type VisibilityCondition
    = Equals String PropValue
    | NotEquals String PropValue
    | Truthy String
    | Not VisibilityCondition
    | And (List VisibilityCondition)
    | Or (List VisibilityCondition)


type alias RepeatContext =
    { item : Value
    , index : Int
    , basePath : String
    }


evaluate : Value -> Maybe RepeatContext -> VisibilityCondition -> Result String Bool
evaluate state repeatCtx condition =
    case condition of
        Truthy path ->
            case State.get path state of
                Just val ->
                    Ok (isTruthy val)

                Nothing ->
                    Ok False

        Equals path expected ->
            case State.get path state of
                Just val ->
                    Ok (propValueMatchesJson expected val)

                Nothing ->
                    Ok False

        NotEquals path expected ->
            case State.get path state of
                Just val ->
                    Ok (not (propValueMatchesJson expected val))

                Nothing ->
                    Ok True

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


propValueMatchesJson : PropValue -> Value -> Bool
propValueMatchesJson propVal jsonVal =
    case propVal of
        StringValue s ->
            Decode.decodeValue Decode.string jsonVal == Ok s

        IntValue i ->
            Decode.decodeValue Decode.int jsonVal == Ok i

        FloatValue f ->
            Decode.decodeValue Decode.float jsonVal == Ok f

        BoolValue b ->
            Decode.decodeValue Decode.bool jsonVal == Ok b

        NullValue ->
            Decode.decodeValue (Decode.null ()) jsonVal == Ok ()

        _ ->
            False


decoder : Decoder VisibilityCondition
decoder =
    Decode.oneOf
        [ -- { "$state": "/path", ... }
          stateConditionDecoder

        -- { "$and": [...] }
        , Decode.field "$and" (Decode.lazy (\_ -> Decode.list decoder) |> Decode.map And)

        -- { "$or": [...] }
        , Decode.field "$or" (Decode.lazy (\_ -> Decode.list decoder) |> Decode.map Or)

        -- [cond, cond] — implicit AND
        , Decode.list (Decode.lazy (\_ -> decoder)) |> Decode.map And

        -- true / false — literal booleans
        , Decode.bool
            |> Decode.map
                (\b ->
                    if b then
                        And []

                    else
                        Or []
                )
        ]


{-| Decode a `{ "$state": "/path", ... }` condition.
Optional modifiers: "eq", "neq", "not".
-}
stateConditionDecoder : Decoder VisibilityCondition
stateConditionDecoder =
    Decode.field "$state" Decode.string
        |> Decode.andThen
            (\path ->
                Decode.oneOf
                    [ Decode.field "eq" PropValue.decoder
                        |> Decode.andThen (\val -> maybeNot (Equals path val))
                    , Decode.field "neq" PropValue.decoder
                        |> Decode.andThen (\val -> maybeNot (NotEquals path val))
                    , maybeNot (Truthy path)
                    ]
            )


{-| Wrap a condition in Not if the "not" field is true.
-}
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
