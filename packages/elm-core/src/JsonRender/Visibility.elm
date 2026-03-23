module JsonRender.Visibility exposing
    ( VisibilityCondition(..)
    , evaluate
    , decoder
    )

{-| Conditional visibility for spec elements.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
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
    }


evaluate : Value -> Maybe RepeatContext -> VisibilityCondition -> Bool
evaluate state repeatCtx condition =
    case condition of
        Truthy path ->
            case State.get path state of
                Just val ->
                    isTruthy val

                Nothing ->
                    False

        Equals path expected ->
            case State.get path state of
                Just val ->
                    propValueMatchesJson expected val

                Nothing ->
                    False

        NotEquals path expected ->
            case State.get path state of
                Just val ->
                    not (propValueMatchesJson expected val)

                Nothing ->
                    True

        Not inner ->
            not (evaluate state repeatCtx inner)

        And conditions ->
            List.all (evaluate state repeatCtx) conditions

        Or conditions ->
            List.any (evaluate state repeatCtx) conditions


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
        [ Decode.field "equals"
            (Decode.succeed Equals
                |> required "path" Decode.string
                |> required "value" PropValue.decoder
            )
        , Decode.field "notEquals"
            (Decode.succeed NotEquals
                |> required "path" Decode.string
                |> required "value" PropValue.decoder
            )
        , Decode.field "truthy" (Decode.string |> Decode.map Truthy)
        , Decode.field "not" (Decode.lazy (\_ -> decoder) |> Decode.map Not)
        , Decode.field "and" (Decode.lazy (\_ -> Decode.list decoder) |> Decode.map And)
        , Decode.field "or" (Decode.lazy (\_ -> Decode.list decoder) |> Decode.map Or)
        ]
