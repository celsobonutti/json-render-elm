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
    resolvedToValueHelp resolved []


type ToValueFrame
    = ToValueListFrame (List ResolvedValue) (List Value)
    | ToValueObjectFrame String (List ( String, ResolvedValue )) (List ( String, Value ))


resolvedToValueHelp : ResolvedValue -> List ToValueFrame -> Value
resolvedToValueHelp current stack =
    let
        leaf val =
            returnToValue val stack
    in
    case current of
        RString s ->
            leaf (Encode.string s)

        RInt i ->
            leaf (Encode.int i)

        RFloat f ->
            leaf (Encode.float f)

        RBool b ->
            leaf (Encode.bool b)

        RNull ->
            leaf Encode.null

        RError err ->
            leaf (Encode.string ("ERROR: " ++ err))

        RList items ->
            case items of
                [] ->
                    leaf (Encode.list identity [])

                first :: rest ->
                    resolvedToValueHelp first (ToValueListFrame rest [] :: stack)

        RObject obj ->
            case Dict.toList obj of
                [] ->
                    leaf (Encode.object [])

                ( k, v ) :: rest ->
                    resolvedToValueHelp v (ToValueObjectFrame k rest [] :: stack)


returnToValue : Value -> List ToValueFrame -> Value
returnToValue val stack =
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
                    returnToValue (Encode.list identity (List.reverse newDone)) rest

                next :: more ->
                    resolvedToValueHelp next (ToValueListFrame more newDone :: rest)

        (ToValueObjectFrame key remaining done) :: rest ->
            let
                newDone =
                    ( key, val ) :: done
            in
            case remaining of
                [] ->
                    returnToValue (Encode.object (List.reverse newDone)) rest

                ( nextKey, nextVal ) :: more ->
                    resolvedToValueHelp nextVal (ToValueObjectFrame nextKey more newDone :: rest)


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
    resolvePropHelp functions state repeatCtx prop []


type PropFrame
    = PropListFrame (List PropValue) (List ResolvedValue)
    | PropObjectFrame String (List ( String, PropValue )) (List ( String, ResolvedValue ))
    | PropCondFrame PropValue PropValue
    | PropComputedFrame String String (List ( String, PropValue )) (List ( String, ResolvedValue ))


resolvePropHelp : FunctionDict -> Value -> Maybe RepeatContext -> PropValue -> List PropFrame -> ResolvedValue
resolvePropHelp functions state repeatCtx prop stack =
    let
        leaf rv =
            returnPropValue functions state repeatCtx rv stack
    in
    case prop of
        StringValue s ->
            leaf (RString s)

        IntValue i ->
            leaf (RInt i)

        FloatValue f ->
            leaf (RFloat f)

        BoolValue b ->
            leaf (RBool b)

        NullValue ->
            leaf RNull

        ListValue items ->
            case items of
                [] ->
                    leaf (RList [])

                first :: rest ->
                    resolvePropHelp functions state repeatCtx first (PropListFrame rest [] :: stack)

        ObjectValue obj ->
            case Dict.toList obj of
                [] ->
                    leaf (RObject Dict.empty)

                ( k, v ) :: rest ->
                    resolvePropHelp functions state repeatCtx v (PropObjectFrame k rest [] :: stack)

        StateExpr path ->
            leaf (jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null))

        ItemExpr field ->
            case repeatCtx of
                Just ctx ->
                    if field == "" then
                        leaf (jsonValueToResolved ctx.item)

                    else
                        case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                            Ok val ->
                                leaf (jsonValueToResolved val)

                            Err _ ->
                                leaf RNull

                Nothing ->
                    leaf RNull

        IndexExpr ->
            case repeatCtx of
                Just ctx ->
                    leaf (RInt ctx.index)

                Nothing ->
                    leaf (RInt 0)

        TemplateExpr template ->
            leaf (RString (resolveTemplate state template))

        BindStateExpr path ->
            leaf (jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null))

        BindItemExpr field ->
            case repeatCtx of
                Just ctx ->
                    if field == "" then
                        leaf (jsonValueToResolved ctx.item)

                    else
                        case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                            Ok val ->
                                leaf (jsonValueToResolved val)

                            Err _ ->
                                leaf RNull

                Nothing ->
                    leaf RNull

        ConditionalExpr cond thenVal elseVal ->
            resolvePropHelp functions state repeatCtx cond (PropCondFrame thenVal elseVal :: stack)

        ComputedExpr name args ->
            case Dict.toList args of
                [] ->
                    case Dict.get name functions of
                        Just fn ->
                            leaf (fn Dict.empty)

                        Nothing ->
                            leaf (RError ("Unknown function: " ++ name))

                ( k, v ) :: rest ->
                    resolvePropHelp functions state repeatCtx v (PropComputedFrame name k rest [] :: stack)


returnPropValue : FunctionDict -> Value -> Maybe RepeatContext -> ResolvedValue -> List PropFrame -> ResolvedValue
returnPropValue functions state repeatCtx rv stack =
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
                    returnPropValue functions state repeatCtx (RList (List.reverse newDone)) rest

                next :: more ->
                    resolvePropHelp functions state repeatCtx next (PropListFrame more newDone :: rest)

        (PropObjectFrame key remaining done) :: rest ->
            let
                newDone =
                    ( key, rv ) :: done
            in
            case remaining of
                [] ->
                    returnPropValue functions state repeatCtx (RObject (Dict.fromList newDone)) rest

                ( nextKey, nextVal ) :: more ->
                    resolvePropHelp functions state repeatCtx nextVal (PropObjectFrame nextKey more newDone :: rest)

        (PropCondFrame thenVal elseVal) :: rest ->
            case isResolvedTruthy rv of
                Ok True ->
                    resolvePropHelp functions state repeatCtx thenVal rest

                Ok False ->
                    resolvePropHelp functions state repeatCtx elseVal rest

                Err err ->
                    returnPropValue functions state repeatCtx (RError err) rest

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
                            returnPropValue functions state repeatCtx err rest

                        Nothing ->
                            case Dict.get name functions of
                                Just fn ->
                                    returnPropValue functions state repeatCtx (fn resolvedArgs) rest

                                Nothing ->
                                    returnPropValue functions state repeatCtx (RError ("Unknown function: " ++ name)) rest

                ( nextKey, nextVal ) :: more ->
                    resolvePropHelp functions state repeatCtx nextVal (PropComputedFrame name nextKey more newDone :: rest)


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
    jsonToResolvedHelp value []


type FromValueFrame
    = FromValueListFrame (List Value) (List ResolvedValue)
    | FromValueObjectFrame String (List ( String, Value )) (List ( String, ResolvedValue ))


jsonToResolvedHelp : Value -> List FromValueFrame -> ResolvedValue
jsonToResolvedHelp value stack =
    let
        leaf rv =
            returnFromValue rv stack
    in
    case Decode.decodeValue Decode.string value of
        Ok s ->
            leaf (RString s)

        Err _ ->
            case Decode.decodeValue Decode.int value of
                Ok i ->
                    leaf (RInt i)

                Err _ ->
                    case Decode.decodeValue Decode.float value of
                        Ok f ->
                            leaf (RFloat f)

                        Err _ ->
                            case Decode.decodeValue Decode.bool value of
                                Ok b ->
                                    leaf (RBool b)

                                Err _ ->
                                    case Decode.decodeValue (Decode.null ()) value of
                                        Ok _ ->
                                            leaf RNull

                                        Err _ ->
                                            case Decode.decodeValue (Decode.list Decode.value) value of
                                                Ok items ->
                                                    case items of
                                                        [] ->
                                                            leaf (RList [])

                                                        first :: rest ->
                                                            jsonToResolvedHelp first (FromValueListFrame rest [] :: stack)

                                                Err _ ->
                                                    case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                                                        Ok pairs ->
                                                            case pairs of
                                                                [] ->
                                                                    leaf (RObject Dict.empty)

                                                                ( k, v ) :: rest ->
                                                                    jsonToResolvedHelp v (FromValueObjectFrame k rest [] :: stack)

                                                        Err _ ->
                                                            leaf RNull


returnFromValue : ResolvedValue -> List FromValueFrame -> ResolvedValue
returnFromValue rv stack =
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
                    returnFromValue (RList (List.reverse newDone)) rest

                next :: more ->
                    jsonToResolvedHelp next (FromValueListFrame more newDone :: rest)

        (FromValueObjectFrame key remaining done) :: rest ->
            let
                newDone =
                    ( key, rv ) :: done
            in
            case remaining of
                [] ->
                    returnFromValue (RObject (Dict.fromList (List.reverse newDone))) rest

                ( nextKey, nextVal ) :: more ->
                    jsonToResolvedHelp nextVal (FromValueObjectFrame nextKey more newDone :: rest)


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
