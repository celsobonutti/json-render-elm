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
resolvedToValue resolved =
    resolvedToValueLoop (ToValueDescend resolved) []


type ToValueStep
    = ToValueDescend ResolvedValue
    | ToValueAscend Value


type ToValueFrame
    = ToValueListFrame (List ResolvedValue) (List Value)
    | ToValueObjectFrame String (List ( String, ResolvedValue )) (List ( String, Value ))


resolvedToValueLoop : ToValueStep -> List ToValueFrame -> Value
resolvedToValueLoop step stack =
    case step of
        ToValueDescend current ->
            case current of
                RString s ->
                    resolvedToValueLoop (ToValueAscend (Encode.string s)) stack

                RInt i ->
                    resolvedToValueLoop (ToValueAscend (Encode.int i)) stack

                RFloat f ->
                    resolvedToValueLoop (ToValueAscend (Encode.float f)) stack

                RBool b ->
                    resolvedToValueLoop (ToValueAscend (Encode.bool b)) stack

                RNull ->
                    resolvedToValueLoop (ToValueAscend Encode.null) stack

                RError err ->
                    resolvedToValueLoop (ToValueAscend (Encode.string ("ERROR: " ++ err))) stack

                RList items ->
                    case items of
                        [] ->
                            resolvedToValueLoop (ToValueAscend (Encode.list identity [])) stack

                        first :: rest ->
                            resolvedToValueLoop (ToValueDescend first) (ToValueListFrame rest [] :: stack)

                RObject obj ->
                    case Dict.toList obj of
                        [] ->
                            resolvedToValueLoop (ToValueAscend (Encode.object [])) stack

                        ( k, v ) :: rest ->
                            resolvedToValueLoop (ToValueDescend v) (ToValueObjectFrame k rest [] :: stack)

        ToValueAscend val ->
            case stack of
                [] ->
                    val

                (ToValueListFrame remaining done) :: rest ->
                    let
                        newDone =
                            val :: done
                    in
                    case remaining of
                        [] ->
                            resolvedToValueLoop (ToValueAscend (Encode.list identity (List.reverse newDone))) rest

                        next :: more ->
                            resolvedToValueLoop (ToValueDescend next) (ToValueListFrame more newDone :: rest)

                (ToValueObjectFrame key remaining done) :: rest ->
                    let
                        newDone =
                            ( key, val ) :: done
                    in
                    case remaining of
                        [] ->
                            resolvedToValueLoop (ToValueAscend (Encode.object (List.reverse newDone))) rest

                        ( nextKey, nextVal ) :: more ->
                            resolvedToValueLoop (ToValueDescend nextVal) (ToValueObjectFrame nextKey more newDone :: rest)


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
resolvePropValueWith functions state repeatCtx prop =
    resolvePropLoop functions state repeatCtx (PropDescend prop) []


type PropStep
    = PropDescend PropValue
    | PropAscend ResolvedValue


type PropFrame
    = PropListFrame (List PropValue) (List ResolvedValue)
    | PropObjectFrame String (List ( String, PropValue )) (List ( String, ResolvedValue ))
    | PropCondFrame PropValue PropValue
    | PropComputedFrame String String (List ( String, PropValue )) (List ( String, ResolvedValue ))


resolvePropLoop : FunctionDict -> Value -> Maybe RepeatContext -> PropStep -> List PropFrame -> ResolvedValue
resolvePropLoop functions state repeatCtx step stack =
    case step of
        PropDescend prop ->
            case prop of
                StringValue s ->
                    resolvePropLoop functions state repeatCtx (PropAscend (RString s)) stack

                IntValue i ->
                    resolvePropLoop functions state repeatCtx (PropAscend (RInt i)) stack

                FloatValue f ->
                    resolvePropLoop functions state repeatCtx (PropAscend (RFloat f)) stack

                BoolValue b ->
                    resolvePropLoop functions state repeatCtx (PropAscend (RBool b)) stack

                NullValue ->
                    resolvePropLoop functions state repeatCtx (PropAscend RNull) stack

                ListValue items ->
                    case items of
                        [] ->
                            resolvePropLoop functions state repeatCtx (PropAscend (RList [])) stack

                        first :: rest ->
                            resolvePropLoop functions state repeatCtx (PropDescend first) (PropListFrame rest [] :: stack)

                ObjectValue obj ->
                    case Dict.toList obj of
                        [] ->
                            resolvePropLoop functions state repeatCtx (PropAscend (RObject Dict.empty)) stack

                        ( k, v ) :: rest ->
                            resolvePropLoop functions state repeatCtx (PropDescend v) (PropObjectFrame k rest [] :: stack)

                StateExpr path ->
                    resolvePropLoop functions state repeatCtx (PropAscend (jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null))) stack

                ItemExpr field ->
                    case repeatCtx of
                        Just ctx ->
                            if field == "" then
                                resolvePropLoop functions state repeatCtx (PropAscend (jsonValueToResolved ctx.item)) stack

                            else
                                case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                                    Ok val ->
                                        resolvePropLoop functions state repeatCtx (PropAscend (jsonValueToResolved val)) stack

                                    Err _ ->
                                        resolvePropLoop functions state repeatCtx (PropAscend RNull) stack

                        Nothing ->
                            resolvePropLoop functions state repeatCtx (PropAscend RNull) stack

                IndexExpr ->
                    case repeatCtx of
                        Just ctx ->
                            resolvePropLoop functions state repeatCtx (PropAscend (RInt ctx.index)) stack

                        Nothing ->
                            resolvePropLoop functions state repeatCtx (PropAscend (RInt 0)) stack

                TemplateExpr template ->
                    resolvePropLoop functions state repeatCtx (PropAscend (RString (resolveTemplate state template))) stack

                BindStateExpr path ->
                    resolvePropLoop functions state repeatCtx (PropAscend (jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null))) stack

                BindItemExpr field ->
                    case repeatCtx of
                        Just ctx ->
                            if field == "" then
                                resolvePropLoop functions state repeatCtx (PropAscend (jsonValueToResolved ctx.item)) stack

                            else
                                case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                                    Ok val ->
                                        resolvePropLoop functions state repeatCtx (PropAscend (jsonValueToResolved val)) stack

                                    Err _ ->
                                        resolvePropLoop functions state repeatCtx (PropAscend RNull) stack

                        Nothing ->
                            resolvePropLoop functions state repeatCtx (PropAscend RNull) stack

                ConditionalExpr cond thenVal elseVal ->
                    resolvePropLoop functions state repeatCtx (PropDescend cond) (PropCondFrame thenVal elseVal :: stack)

                ComputedExpr name args ->
                    case Dict.toList args of
                        [] ->
                            case Dict.get name functions of
                                Just fn ->
                                    resolvePropLoop functions state repeatCtx (PropAscend (fn Dict.empty)) stack

                                Nothing ->
                                    resolvePropLoop functions state repeatCtx (PropAscend (RError ("Unknown function: " ++ name))) stack

                        ( k, v ) :: rest ->
                            resolvePropLoop functions state repeatCtx (PropDescend v) (PropComputedFrame name k rest [] :: stack)

        PropAscend rv ->
            case stack of
                [] ->
                    rv

                (PropListFrame remaining done) :: rest ->
                    let
                        newDone =
                            rv :: done
                    in
                    case remaining of
                        [] ->
                            resolvePropLoop functions state repeatCtx (PropAscend (RList (List.reverse newDone))) rest

                        next :: more ->
                            resolvePropLoop functions state repeatCtx (PropDescend next) (PropListFrame more newDone :: rest)

                (PropObjectFrame key remaining done) :: rest ->
                    let
                        newDone =
                            ( key, rv ) :: done
                    in
                    case remaining of
                        [] ->
                            resolvePropLoop functions state repeatCtx (PropAscend (RObject (Dict.fromList newDone))) rest

                        ( nextKey, nextVal ) :: more ->
                            resolvePropLoop functions state repeatCtx (PropDescend nextVal) (PropObjectFrame nextKey more newDone :: rest)

                (PropCondFrame thenVal elseVal) :: rest ->
                    case isResolvedTruthy rv of
                        Ok True ->
                            resolvePropLoop functions state repeatCtx (PropDescend thenVal) rest

                        Ok False ->
                            resolvePropLoop functions state repeatCtx (PropDescend elseVal) rest

                        Err err ->
                            resolvePropLoop functions state repeatCtx (PropAscend (RError err)) rest

                (PropComputedFrame name key remaining done) :: rest ->
                    let
                        newDone =
                            ( key, rv ) :: done
                    in
                    case remaining of
                        [] ->
                            let
                                resolvedArgs =
                                    Dict.fromList newDone
                            in
                            case findError resolvedArgs of
                                Just err ->
                                    resolvePropLoop functions state repeatCtx (PropAscend err) rest

                                Nothing ->
                                    case Dict.get name functions of
                                        Just fn ->
                                            resolvePropLoop functions state repeatCtx (PropAscend (fn resolvedArgs)) rest

                                        Nothing ->
                                            resolvePropLoop functions state repeatCtx (PropAscend (RError ("Unknown function: " ++ name))) rest

                        ( nextKey, nextVal ) :: more ->
                            resolvePropLoop functions state repeatCtx (PropDescend nextVal) (PropComputedFrame name nextKey more newDone :: rest)


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
jsonValueToResolved value =
    fromValueLoop (FromValueDescend value) []


type FromValueStep
    = FromValueDescend Value
    | FromValueAscend ResolvedValue


type FromValueFrame
    = FromValueListFrame (List Value) (List ResolvedValue)
    | FromValueObjectFrame String (List ( String, Value )) (List ( String, ResolvedValue ))


fromValueLoop : FromValueStep -> List FromValueFrame -> ResolvedValue
fromValueLoop step stack =
    case step of
        FromValueDescend value ->
            case Decode.decodeValue Decode.string value of
                Ok s ->
                    fromValueLoop (FromValueAscend (RString s)) stack

                Err _ ->
                    case Decode.decodeValue Decode.int value of
                        Ok i ->
                            fromValueLoop (FromValueAscend (RInt i)) stack

                        Err _ ->
                            case Decode.decodeValue Decode.float value of
                                Ok f ->
                                    fromValueLoop (FromValueAscend (RFloat f)) stack

                                Err _ ->
                                    case Decode.decodeValue Decode.bool value of
                                        Ok b ->
                                            fromValueLoop (FromValueAscend (RBool b)) stack

                                        Err _ ->
                                            case Decode.decodeValue (Decode.null ()) value of
                                                Ok _ ->
                                                    fromValueLoop (FromValueAscend RNull) stack

                                                Err _ ->
                                                    case Decode.decodeValue (Decode.list Decode.value) value of
                                                        Ok items ->
                                                            case items of
                                                                [] ->
                                                                    fromValueLoop (FromValueAscend (RList [])) stack

                                                                first :: rest ->
                                                                    fromValueLoop (FromValueDescend first) (FromValueListFrame rest [] :: stack)

                                                        Err _ ->
                                                            case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                                                                Ok pairs ->
                                                                    case pairs of
                                                                        [] ->
                                                                            fromValueLoop (FromValueAscend (RObject Dict.empty)) stack

                                                                        ( k, v ) :: rest ->
                                                                            fromValueLoop (FromValueDescend v) (FromValueObjectFrame k rest [] :: stack)

                                                                Err _ ->
                                                                    fromValueLoop (FromValueAscend RNull) stack

        FromValueAscend rv ->
            case stack of
                [] ->
                    rv

                (FromValueListFrame remaining done) :: rest ->
                    let
                        newDone =
                            rv :: done
                    in
                    case remaining of
                        [] ->
                            fromValueLoop (FromValueAscend (RList (List.reverse newDone))) rest

                        next :: more ->
                            fromValueLoop (FromValueDescend next) (FromValueListFrame more newDone :: rest)

                (FromValueObjectFrame key remaining done) :: rest ->
                    let
                        newDone =
                            ( key, rv ) :: done
                    in
                    case remaining of
                        [] ->
                            fromValueLoop (FromValueAscend (RObject (Dict.fromList (List.reverse newDone)))) rest

                        ( nextKey, nextVal ) :: more ->
                            fromValueLoop (FromValueDescend nextVal) (FromValueObjectFrame nextKey more newDone :: rest)


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
