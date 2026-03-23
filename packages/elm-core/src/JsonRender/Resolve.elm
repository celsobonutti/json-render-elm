module JsonRender.Resolve exposing
    ( ResolvedValue(..)
    , RepeatContext
    , resolveProps
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


type alias RepeatContext =
    { item : Value
    , index : Int
    }


type alias PropsDecoder a =
    Dict String ResolvedValue -> Result String a


resolveProps : Value -> Maybe RepeatContext -> Dict String PropValue -> Dict String ResolvedValue
resolveProps state repeatCtx props =
    Dict.map (\_ v -> resolvePropValue state repeatCtx v) props


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

                        Err _ ->
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

        _ ->
            Err "expected string"


int : ResolvedValue -> Result String Int
int val =
    case val of
        RInt i ->
            Ok i

        _ ->
            Err "expected int"


float : ResolvedValue -> Result String Float
float val =
    case val of
        RFloat f ->
            Ok f

        RInt i ->
            Ok (toFloat i)

        _ ->
            Err "expected float"


bool : ResolvedValue -> Result String Bool
bool val =
    case val of
        RBool b ->
            Ok b

        _ ->
            Err "expected bool"
