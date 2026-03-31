module JsonRender.Resolve exposing
    ( FunctionDict
    , RepeatContext
    , ResolvedValue(..)
    , bool
    , float
    , int
    , optional
    , required
    , resolveActionParams
    , resolveActionParamsWith
    , resolvePropValue
    , resolveProps
    , resolvePropsWith
    , resolvedToValue
    , string
    , succeed
    )

{-| Expression evaluation and pipeline-style prop decoders.
-}

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.State as State
import Recursion
import Recursion.Traverse


type ResolvedValue
    = RString String
    | RInt Int
    | RFloat Float
    | RBool Bool
    | RNull
    | RList (List ResolvedValue)
    | RObject (Dict String ResolvedValue)
    | RError String


type alias RepeatContext =
    { item : Value
    , index : Int
    , basePath : String
    }


type alias FunctionDict =
    Dict String (Dict String ResolvedValue -> ResolvedValue)


type alias PropsDecoder a =
    Dict String ResolvedValue -> Result String a


resolvePropsWith : FunctionDict -> Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String ResolvedValue
resolvePropsWith functions state repeatCtx props =
    Dict.map (\_ v -> resolvePropValueWith functions state repeatCtx v) props


resolveProps : Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String ResolvedValue
resolveProps =
    resolvePropsWith Dict.empty


{-| Convert a ResolvedValue back to a JSON Value. Inverse of jsonValueToResolved.
-}
resolvedToValue : ResolvedValue -> Value
resolvedToValue =
    Recursion.runRecursion <|
        \resolved ->
            case resolved of
                RString s ->
                    Recursion.base (Encode.string s)

                RInt i ->
                    Recursion.base (Encode.int i)

                RFloat f ->
                    Recursion.base (Encode.float f)

                RBool b ->
                    Recursion.base (Encode.bool b)

                RNull ->
                    Recursion.base Encode.null

                RError err ->
                    Recursion.base (Encode.string ("ERROR: " ++ err))

                RList items ->
                    Recursion.Traverse.sequenceList items
                        |> Recursion.map (Encode.list identity)

                RObject obj ->
                    Recursion.Traverse.sequenceDict obj
                        |> Recursion.map (Dict.toList >> Encode.object)


{-| Resolve action params: resolve each PropValue to a JSON Value.
In action params, `$item` resolves to the absolute state path (e.g. "/todos/0/completed")
rather than the field's value, per the json-render prompt spec.
-}
resolveActionParamsWith : FunctionDict -> Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String Value
resolveActionParamsWith functions state repeatCtx params =
    params
        |> Dict.map (\_ v -> resolveActionParamValueWith functions state repeatCtx v)
        |> Dict.map (\_ v -> resolvedToValue v)


resolveActionParams : Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String Value
resolveActionParams =
    resolveActionParamsWith Dict.empty


{-| Like resolvePropValueWith but $item resolves to the absolute state path.
$cond conditions still use value-based resolution for truthiness checks.
-}
resolveActionParamValueWith : FunctionDict -> Value -> Maybe RepeatContext -> PropValue -> ResolvedValue
resolveActionParamValueWith functions state repeatCtx prop =
    case prop of
        ItemExpr field ->
            case repeatCtx of
                Just ctx ->
                    if field == "" then
                        RString ctx.basePath

                    else
                        RString (ctx.basePath ++ "/" ++ field)

                Nothing ->
                    RNull

        ConditionalExpr cond thenVal elseVal ->
            let
                condResolved =
                    resolvePropValueWith functions state repeatCtx cond
            in
            case isResolvedTruthy condResolved of
                Ok True ->
                    resolveActionParamValueWith functions state repeatCtx thenVal

                Ok False ->
                    resolveActionParamValueWith functions state repeatCtx elseVal

                Err err ->
                    RError err

        _ ->
            resolvePropValueWith functions state repeatCtx prop


resolvePropValue : Value -> Maybe RepeatContext -> PropValue -> ResolvedValue
resolvePropValue =
    resolvePropValueWith Dict.empty


resolvePropValueWith : FunctionDict -> Value -> Maybe RepeatContext -> PropValue -> ResolvedValue
resolvePropValueWith functions state repeatCtx =
    Recursion.runRecursion <|
        \prop ->
            case prop of
                StringValue s ->
                    Recursion.base (RString s)

                IntValue i ->
                    Recursion.base (RInt i)

                FloatValue f ->
                    Recursion.base (RFloat f)

                BoolValue b ->
                    Recursion.base (RBool b)

                NullValue ->
                    Recursion.base RNull

                ListValue items ->
                    Recursion.Traverse.sequenceList items
                        |> Recursion.map RList

                ObjectValue obj ->
                    Recursion.Traverse.sequenceDict obj
                        |> Recursion.map RObject

                StateExpr path ->
                    Recursion.base (jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null))

                ItemExpr field ->
                    case repeatCtx of
                        Just ctx ->
                            if field == "" then
                                Recursion.base (jsonValueToResolved ctx.item)

                            else
                                case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                                    Ok val ->
                                        Recursion.base (jsonValueToResolved val)

                                    Err _ ->
                                        Recursion.base RNull

                        Nothing ->
                            Recursion.base RNull

                IndexExpr ->
                    case repeatCtx of
                        Just ctx ->
                            Recursion.base (RInt ctx.index)

                        Nothing ->
                            Recursion.base (RInt 0)

                TemplateExpr template ->
                    Recursion.base (RString (resolveTemplate state template))

                BindStateExpr path ->
                    Recursion.base (jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null))

                BindItemExpr field ->
                    case repeatCtx of
                        Just ctx ->
                            if field == "" then
                                Recursion.base (jsonValueToResolved ctx.item)

                            else
                                case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                                    Ok val ->
                                        Recursion.base (jsonValueToResolved val)

                                    Err _ ->
                                        Recursion.base RNull

                        Nothing ->
                            Recursion.base RNull

                ConditionalExpr cond thenVal elseVal ->
                    Recursion.recurseThen cond <|
                        \condResolved ->
                            case isResolvedTruthy condResolved of
                                Ok True ->
                                    Recursion.recurse thenVal

                                Ok False ->
                                    Recursion.recurse elseVal

                                Err err ->
                                    Recursion.base (RError err)

                ComputedExpr name args ->
                    Recursion.Traverse.sequenceDictThen args <|
                        \resolvedArgs ->
                            case findError resolvedArgs of
                                Just err ->
                                    Recursion.base err

                                Nothing ->
                                    case Dict.get name functions of
                                        Just fn ->
                                            Recursion.base (fn resolvedArgs)

                                        Nothing ->
                                            Recursion.base (RError ("Unknown function: " ++ name))


findError : Dict String ResolvedValue -> Maybe ResolvedValue
findError dict =
    Dict.foldl
        (\_ v acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case v of
                        RError _ ->
                            Just v

                        _ ->
                            Nothing
        )
        Nothing
        dict


isResolvedTruthy : ResolvedValue -> Result String Bool
isResolvedTruthy resolved =
    case resolved of
        RBool b ->
            Ok b

        RString s ->
            Ok (s /= "")

        RInt n ->
            Ok (n /= 0)

        RFloat f ->
            Ok (f /= 0.0)

        RNull ->
            Ok False

        RList items ->
            Ok (not (List.isEmpty items))

        RObject _ ->
            Ok True

        RError err ->
            Err err


jsonValueToResolved : Value -> ResolvedValue
jsonValueToResolved =
    Recursion.runRecursion <|
        \value ->
            case Decode.decodeValue Decode.string value of
                Ok s ->
                    Recursion.base (RString s)

                Err _ ->
                    case Decode.decodeValue Decode.int value of
                        Ok i ->
                            Recursion.base (RInt i)

                        Err _ ->
                            case Decode.decodeValue Decode.float value of
                                Ok f ->
                                    Recursion.base (RFloat f)

                                Err _ ->
                                    case Decode.decodeValue Decode.bool value of
                                        Ok b ->
                                            Recursion.base (RBool b)

                                        Err _ ->
                                            case Decode.decodeValue (Decode.null ()) value of
                                                Ok _ ->
                                                    Recursion.base RNull

                                                Err _ ->
                                                    case Decode.decodeValue (Decode.list Decode.value) value of
                                                        Ok items ->
                                                            Recursion.Traverse.sequenceList items
                                                                |> Recursion.map RList

                                                        Err _ ->
                                                            case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                                                                Ok pairs ->
                                                                    Recursion.Traverse.sequenceDict (Dict.fromList pairs)
                                                                        |> Recursion.map RObject

                                                                Err _ ->
                                                                    Recursion.base RNull


resolveTemplate : Value -> String -> String
resolveTemplate state template =
    let
        parts =
            String.split "${" template
    in
    case parts of
        [] ->
            template

        first :: rest ->
            first
                ++ String.concat
                    (List.map
                        (\part ->
                            case String.split "}" part of
                                [ path, after ] ->
                                    let
                                        resolved =
                                            State.get path state
                                                |> Maybe.map jsonValueToString
                                                |> Maybe.withDefault ""
                                    in
                                    resolved ++ after

                                _ ->
                                    "${" ++ part
                        )
                        rest
                    )


jsonValueToString : Value -> String
jsonValueToString value =
    case Decode.decodeValue Decode.string value of
        Ok s ->
            s

        Err _ ->
            Encode.encode 0 value



-- Pipeline decoders


succeed : a -> PropsDecoder a
succeed a _ =
    Ok a


required : String -> (ResolvedValue -> Result String b) -> PropsDecoder (b -> c) -> PropsDecoder c
required key extract prev props =
    case prev props of
        Ok f ->
            case Dict.get key props of
                Just val ->
                    case extract val of
                        Ok extracted ->
                            Ok (f extracted)

                        Err err ->
                            Err ("field '" ++ key ++ "': " ++ err)

                Nothing ->
                    Err ("missing required field '" ++ key ++ "'")

        Err err ->
            Err err


optional : String -> (ResolvedValue -> Result String b) -> Maybe b -> PropsDecoder (Maybe b -> c) -> PropsDecoder c
optional key extract default prev props =
    case prev props of
        Ok f ->
            case Dict.get key props of
                Just val ->
                    case extract val of
                        Ok extracted ->
                            Ok (f (Just extracted))

                        Err err ->
                            case val of
                                RError _ ->
                                    Err err

                                _ ->
                                    Ok (f default)

                Nothing ->
                    Ok (f default)

        Err err ->
            Err err


string : ResolvedValue -> Result String String
string val =
    case val of
        RString s ->
            Ok s

        RError err ->
            Err err

        _ ->
            Err "expected string"


int : ResolvedValue -> Result String Int
int val =
    case val of
        RInt i ->
            Ok i

        RError err ->
            Err err

        _ ->
            Err "expected int"


float : ResolvedValue -> Result String Float
float val =
    case val of
        RFloat f ->
            Ok f

        RInt i ->
            Ok (toFloat i)

        RError err ->
            Err err

        _ ->
            Err "expected float"


bool : ResolvedValue -> Result String Bool
bool val =
    case val of
        RBool b ->
            Ok b

        RError err ->
            Err err

        _ ->
            Err "expected bool"
