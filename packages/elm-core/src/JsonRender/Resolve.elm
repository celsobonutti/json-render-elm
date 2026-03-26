module JsonRender.Resolve exposing
    ( ResolvedValue(..)
    , RepeatContext
    , resolveActionParams
    , resolveProps
    , resolvedToValue
    , succeed
    , required
    , optional
    , string
    , int
    , float
    , bool
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


type alias PropsDecoder a =
    Dict String ResolvedValue -> Result String a


resolveProps : Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String ResolvedValue
resolveProps state repeatCtx props =
    Dict.map (\_ v -> resolvePropValue state repeatCtx v) props


{-| Convert a ResolvedValue back to a JSON Value. Inverse of jsonValueToResolved.
-}
resolvedToValue : ResolvedValue -> Value
resolvedToValue resolved =
    case resolved of
        RString s ->
            Encode.string s

        RInt i ->
            Encode.int i

        RFloat f ->
            Encode.float f

        RBool b ->
            Encode.bool b

        RNull ->
            Encode.null

        RList items ->
            Encode.list resolvedToValue items

        RObject obj ->
            obj
                |> Dict.toList
                |> List.map (\( k, v ) -> ( k, resolvedToValue v ))
                |> Encode.object

        RError err ->
            Encode.string ("ERROR: " ++ err)


{-| Resolve action params: resolve each PropValue to a JSON Value.
In action params, `$item` resolves to the absolute state path (e.g. "/todos/0/completed")
rather than the field's value, per the json-render prompt spec.
-}
resolveActionParams : Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String Value
resolveActionParams state repeatCtx params =
    params
        |> Dict.map (\_ v -> resolveActionParamValue state repeatCtx v)
        |> Dict.map (\_ v -> resolvedToValue v)


{-| Like resolvePropValue but $item resolves to the absolute state path.
$cond conditions still use value-based resolution for truthiness checks.
-}
resolveActionParamValue : Value -> Maybe RepeatContext -> PropValue -> ResolvedValue
resolveActionParamValue state repeatCtx prop =
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
                    resolvePropValue state repeatCtx cond
            in
            case isResolvedTruthy condResolved of
                Ok True ->
                    resolveActionParamValue state repeatCtx thenVal

                Ok False ->
                    resolveActionParamValue state repeatCtx elseVal

                Err err ->
                    RError err

        _ ->
            resolvePropValue state repeatCtx prop


resolvePropValue : Value -> Maybe RepeatContext -> PropValue -> ResolvedValue
resolvePropValue state repeatCtx prop =
    case prop of
        StringValue s ->
            RString s

        IntValue i ->
            RInt i

        FloatValue f ->
            RFloat f

        BoolValue b ->
            RBool b

        NullValue ->
            RNull

        ListValue items ->
            RList (List.map (resolvePropValue state repeatCtx) items)

        ObjectValue obj ->
            RObject (Dict.map (\_ v -> resolvePropValue state repeatCtx v) obj)

        StateExpr path ->
            jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null)

        ItemExpr field ->
            case repeatCtx of
                Just ctx ->
                    if field == "" then
                        jsonValueToResolved ctx.item

                    else
                        case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                            Ok val ->
                                jsonValueToResolved val

                            Err _ ->
                                RNull

                Nothing ->
                    RNull

        IndexExpr ->
            case repeatCtx of
                Just ctx ->
                    RInt ctx.index

                Nothing ->
                    RInt 0

        TemplateExpr template ->
            RString (resolveTemplate state template)

        BindStateExpr path ->
            jsonValueToResolved (State.get path state |> Maybe.withDefault Encode.null)

        BindItemExpr field ->
            case repeatCtx of
                Just ctx ->
                    if field == "" then
                        jsonValueToResolved ctx.item

                    else
                        case Decode.decodeValue (Decode.field field Decode.value) ctx.item of
                            Ok val ->
                                jsonValueToResolved val

                            Err _ ->
                                RNull

                Nothing ->
                    RNull

        ConditionalExpr cond thenVal elseVal ->
            let
                condResolved =
                    resolvePropValue state repeatCtx cond
            in
            case isResolvedTruthy condResolved of
                Ok True ->
                    resolvePropValue state repeatCtx thenVal

                Ok False ->
                    resolvePropValue state repeatCtx elseVal

                Err err ->
                    RError err

        ComputedExpr name _ ->
            RError ("$computed not yet supported: " ++ name)


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
    case Decode.decodeValue Decode.string value of
        Ok s ->
            RString s

        Err _ ->
            case Decode.decodeValue Decode.int value of
                Ok i ->
                    RInt i

                Err _ ->
                    case Decode.decodeValue Decode.float value of
                        Ok f ->
                            RFloat f

                        Err _ ->
                            case Decode.decodeValue Decode.bool value of
                                Ok b ->
                                    RBool b

                                Err _ ->
                                    case Decode.decodeValue (Decode.null ()) value of
                                        Ok _ ->
                                            RNull

                                        Err _ ->
                                            case Decode.decodeValue (Decode.list Decode.value) value of
                                                Ok items ->
                                                    RList (List.map jsonValueToResolved items)

                                                Err _ ->
                                                    case Decode.decodeValue (Decode.keyValuePairs Decode.value) value of
                                                        Ok pairs ->
                                                            RObject (Dict.fromList (List.map (\( k, v ) -> ( k, jsonValueToResolved v )) pairs))

                                                        Err _ ->
                                                            RNull


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
