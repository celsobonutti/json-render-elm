module JsonRender.Validation exposing
    ( BuiltInCheck(..)
    , CheckType(..)
    , FieldValidation
    , ValidateOn(..)
    , ValidationCheck
    , ValidationConfig
    , ValidationFunction
    , ValidationFunctionDict
    , ValidationResult
    , checkDecoder
    , checkTypeDecoder
    , extractValidation
    , field
    , runCheck
    , runValidation
    , succeed
    , validateOnDecoder
    )

{-| Form validation types, decoders, check execution, and pipeline combinators.
-}

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import JsonRender.Internal.Condition as Condition exposing (Condition, RepeatContext)
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Resolve as Resolve
import Regex



-- Types


type BuiltInCheck
    = Required
    | Email
    | MinLength
    | MaxLength
    | Pattern
    | Min
    | Max
    | Numeric
    | Url
    | Matches
    | EqualTo
    | LessThan
    | GreaterThan
    | RequiredIf


type CheckType
    = BuiltIn BuiltInCheck
    | Custom String


type ValidateOn
    = OnChange
    | OnBlur
    | OnSubmit


type alias ValidationCheck =
    { type_ : CheckType
    , args : Dict String PropValue
    , message : String
    }


type alias ValidationConfig =
    { checks : List ValidationCheck
    , validateOn : ValidateOn
    , enabled : Maybe Condition
    }


type alias FieldValidation =
    { errors : List String
    , touched : Bool
    , validated : Bool
    }


type alias ValidationResult =
    { valid : Bool
    , errors : List String
    }


type alias ValidationFunction =
    Value -> Dict String Value -> Bool


type alias ValidationFunctionDict =
    Dict String ValidationFunction



-- Decoders


checkTypeDecoder : Decoder CheckType
checkTypeDecoder =
    Decode.string
        |> Decode.map
            (\s ->
                case s of
                    "required" ->
                        BuiltIn Required

                    "email" ->
                        BuiltIn Email

                    "minLength" ->
                        BuiltIn MinLength

                    "maxLength" ->
                        BuiltIn MaxLength

                    "pattern" ->
                        BuiltIn Pattern

                    "min" ->
                        BuiltIn Min

                    "max" ->
                        BuiltIn Max

                    "numeric" ->
                        BuiltIn Numeric

                    "url" ->
                        BuiltIn Url

                    "matches" ->
                        BuiltIn Matches

                    "equalTo" ->
                        BuiltIn EqualTo

                    "lessThan" ->
                        BuiltIn LessThan

                    "greaterThan" ->
                        BuiltIn GreaterThan

                    "requiredIf" ->
                        BuiltIn RequiredIf

                    other ->
                        Custom other
            )


validateOnDecoder : Decoder ValidateOn
validateOnDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "change" ->
                        Decode.succeed OnChange

                    "blur" ->
                        Decode.succeed OnBlur

                    "submit" ->
                        Decode.succeed OnSubmit

                    other ->
                        Decode.fail ("Unknown validateOn: " ++ other)
            )


checkDecoder : Decoder ValidationCheck
checkDecoder =
    Decode.map3 ValidationCheck
        (Decode.field "type" checkTypeDecoder)
        (Decode.oneOf
            [ Decode.field "args" (Decode.dict JsonRender.Internal.PropValue.decoder)
            , Decode.succeed Dict.empty
            ]
        )
        (Decode.field "message" Decode.string)



-- Check execution


{-| Run a single built-in check against a value with resolved args.
Custom checks always pass here (looked up separately in ValidationFunctionDict).
-}
runCheck : CheckType -> Value -> Dict String Value -> Bool
runCheck checkType value args =
    case checkType of
        BuiltIn Required ->
            isRequired value

        BuiltIn Email ->
            checkEmail value

        BuiltIn MinLength ->
            checkMinLength value args

        BuiltIn MaxLength ->
            checkMaxLength value args

        BuiltIn Pattern ->
            checkPattern value args

        BuiltIn Min ->
            checkMin value args

        BuiltIn Max ->
            checkMax value args

        BuiltIn Numeric ->
            checkNumeric value

        BuiltIn Url ->
            checkUrl value

        BuiltIn Matches ->
            checkMatches value args

        BuiltIn EqualTo ->
            checkEqualTo value args

        BuiltIn LessThan ->
            checkLessThan value args

        BuiltIn GreaterThan ->
            checkGreaterThan value args

        BuiltIn RequiredIf ->
            checkRequiredIf value args

        Custom _ ->
            True


isRequired : Value -> Bool
isRequired value =
    case Decode.decodeValue Decode.string value of
        Ok s ->
            String.trim s /= ""

        Err _ ->
            case Decode.decodeValue (Decode.null ()) value of
                Ok _ ->
                    False

                Err _ ->
                    case Decode.decodeValue (Decode.list Decode.value) value of
                        Ok items ->
                            not (List.isEmpty items)

                        Err _ ->
                            True


checkEmail : Value -> Bool
checkEmail value =
    case Decode.decodeValue Decode.string value of
        Ok s ->
            if s == "" then
                True

            else
                let
                    maybeRegex =
                        Regex.fromString "^[^\\s@]+@[^\\s@]+\\.[^\\s@]+$"
                in
                case maybeRegex of
                    Just regex ->
                        Regex.contains regex s

                    Nothing ->
                        False

        Err _ ->
            False


checkMinLength : Value -> Dict String Value -> Bool
checkMinLength value args =
    case ( Decode.decodeValue Decode.string value, getIntArg "value" args ) of
        ( Ok s, Just minLen ) ->
            String.length s >= minLen

        _ ->
            True


checkMaxLength : Value -> Dict String Value -> Bool
checkMaxLength value args =
    case ( Decode.decodeValue Decode.string value, getIntArg "value" args ) of
        ( Ok s, Just maxLen ) ->
            String.length s <= maxLen

        _ ->
            True


checkPattern : Value -> Dict String Value -> Bool
checkPattern value args =
    case ( Decode.decodeValue Decode.string value, getStringArg "value" args ) of
        ( Ok s, Just pat ) ->
            case Regex.fromString pat of
                Just regex ->
                    Regex.contains regex s

                Nothing ->
                    True

        _ ->
            True


checkMin : Value -> Dict String Value -> Bool
checkMin value args =
    case ( toNumber value, getNumberArg "value" args ) of
        ( Just n, Just minVal ) ->
            n >= minVal

        _ ->
            True


checkMax : Value -> Dict String Value -> Bool
checkMax value args =
    case ( toNumber value, getNumberArg "value" args ) of
        ( Just n, Just maxVal ) ->
            n <= maxVal

        _ ->
            True


checkNumeric : Value -> Bool
checkNumeric value =
    case Decode.decodeValue Decode.string value of
        Ok s ->
            if s == "" then
                True

            else
                case String.toFloat s of
                    Just _ ->
                        True

                    Nothing ->
                        False

        Err _ ->
            case Decode.decodeValue Decode.int value of
                Ok _ ->
                    True

                Err _ ->
                    case Decode.decodeValue Decode.float value of
                        Ok _ ->
                            True

                        Err _ ->
                            False


checkUrl : Value -> Bool
checkUrl value =
    case Decode.decodeValue Decode.string value of
        Ok s ->
            if s == "" then
                True

            else
                case Regex.fromString "^https?://.+" of
                    Just regex ->
                        Regex.contains regex s

                    Nothing ->
                        False

        Err _ ->
            False


checkMatches : Value -> Dict String Value -> Bool
checkMatches value args =
    case Dict.get "field" args of
        Just fieldVal ->
            Encode.encode 0 value == Encode.encode 0 fieldVal

        Nothing ->
            True


checkEqualTo : Value -> Dict String Value -> Bool
checkEqualTo value args =
    case Dict.get "field" args of
        Just fieldVal ->
            Encode.encode 0 value == Encode.encode 0 fieldVal

        Nothing ->
            True


checkLessThan : Value -> Dict String Value -> Bool
checkLessThan value args =
    case ( toNumber value, getNumberArg "value" args ) of
        ( Just n, Just threshold ) ->
            n < threshold

        _ ->
            -- String fallback
            case ( Decode.decodeValue Decode.string value, getStringArg "value" args ) of
                ( Ok s1, Just s2 ) ->
                    s1 < s2

                _ ->
                    True


checkGreaterThan : Value -> Dict String Value -> Bool
checkGreaterThan value args =
    case ( toNumber value, getNumberArg "value" args ) of
        ( Just n, Just threshold ) ->
            n > threshold

        _ ->
            -- String fallback
            case ( Decode.decodeValue Decode.string value, getStringArg "value" args ) of
                ( Ok s1, Just s2 ) ->
                    s1 > s2

                _ ->
                    True


checkRequiredIf : Value -> Dict String Value -> Bool
checkRequiredIf value args =
    case Dict.get "field" args of
        Just fieldVal ->
            if isTruthy fieldVal then
                isRequired value

            else
                True

        Nothing ->
            True



-- runValidation


{-| Run all validation checks in a config against a field value.
Checks the `enabled` condition first; if disabled, returns valid.
Resolves check args using Resolve.resolveActionParamsWith before running.
Custom checks are looked up in the ValidationFunctionDict.
-}
runValidation :
    ValidationFunctionDict
    -> Resolve.FunctionDict
    -> ValidationConfig
    -> Value
    -> Value
    -> Maybe RepeatContext
    -> ValidationResult
runValidation validationFns functions config fieldValue state repeatCtx =
    case config.enabled of
        Just condition ->
            case Condition.evaluate state repeatCtx condition of
                Ok True ->
                    runChecks validationFns functions config.checks fieldValue state repeatCtx

                Ok False ->
                    { valid = True, errors = [] }

                Err _ ->
                    { valid = True, errors = [] }

        Nothing ->
            runChecks validationFns functions config.checks fieldValue state repeatCtx


runChecks :
    ValidationFunctionDict
    -> Resolve.FunctionDict
    -> List ValidationCheck
    -> Value
    -> Value
    -> Maybe RepeatContext
    -> ValidationResult
runChecks validationFns functions checks fieldValue state repeatCtx =
    let
        errors =
            List.filterMap
                (\check ->
                    let
                        resolvedArgs =
                            Resolve.resolveActionParamsWith functions state repeatCtx check.args

                        passed =
                            case check.type_ of
                                Custom name ->
                                    case Dict.get name validationFns of
                                        Just fn ->
                                            fn fieldValue resolvedArgs

                                        Nothing ->
                                            True

                                _ ->
                                    runCheck check.type_ fieldValue resolvedArgs
                    in
                    if passed then
                        Nothing

                    else
                        Just check.message
                )
                checks
    in
    { valid = List.isEmpty errors
    , errors = errors
    }



-- Extraction


{-| Extract validation config from a props dict.
Looks for a `checks` ListValue prop (list of ObjectValue checks),
a `validateOn` StringValue prop (default OnSubmit), and finds
the first `$bindState` path in the props dict.
Returns `Just (bindStatePath, validationConfig)` if found, Nothing otherwise.
-}
extractValidation : Maybe Condition -> Dict String PropValue -> Maybe ( String, ValidationConfig )
extractValidation enabledCondition props =
    case ( findBindStatePath props, Dict.get "checks" props ) of
        ( Just path, Just (ListValue checksList) ) ->
            case parseChecks checksList of
                Just parsedChecks ->
                    let
                        validateOn =
                            case Dict.get "validateOn" props of
                                Just (StringValue "change") ->
                                    OnChange

                                Just (StringValue "blur") ->
                                    OnBlur

                                _ ->
                                    OnSubmit
                    in
                    Just
                        ( path
                        , { checks = parsedChecks
                          , validateOn = validateOn
                          , enabled = enabledCondition
                          }
                        )

                Nothing ->
                    Nothing

        _ ->
            Nothing


findBindStatePath : Dict String PropValue -> Maybe String
findBindStatePath props =
    Dict.foldl
        (\_ v acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    case v of
                        BindStateExpr path ->
                            Just path

                        _ ->
                            Nothing
        )
        Nothing
        props


parseChecks : List PropValue -> Maybe (List ValidationCheck)
parseChecks items =
    let
        results =
            List.filterMap parseCheck items
    in
    if List.length results == List.length items then
        Just results

    else
        Nothing


parseCheck : PropValue -> Maybe ValidationCheck
parseCheck pv =
    case pv of
        ObjectValue obj ->
            case Dict.get "type" obj of
                Just (StringValue typeStr) ->
                    let
                        checkType =
                            parseCheckType typeStr

                        message =
                            case Dict.get "message" obj of
                                Just (StringValue msg) ->
                                    msg

                                _ ->
                                    "Validation failed"

                        args =
                            case Dict.get "args" obj of
                                Just (ObjectValue argsDict) ->
                                    argsDict

                                _ ->
                                    Dict.empty
                    in
                    Just
                        { type_ = checkType
                        , args = args
                        , message = message
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


parseCheckType : String -> CheckType
parseCheckType s =
    case s of
        "required" ->
            BuiltIn Required

        "email" ->
            BuiltIn Email

        "minLength" ->
            BuiltIn MinLength

        "maxLength" ->
            BuiltIn MaxLength

        "pattern" ->
            BuiltIn Pattern

        "min" ->
            BuiltIn Min

        "max" ->
            BuiltIn Max

        "numeric" ->
            BuiltIn Numeric

        "url" ->
            BuiltIn Url

        "matches" ->
            BuiltIn Matches

        "equalTo" ->
            BuiltIn EqualTo

        "lessThan" ->
            BuiltIn LessThan

        "greaterThan" ->
            BuiltIn GreaterThan

        "requiredIf" ->
            BuiltIn RequiredIf

        other ->
            Custom other



-- Pipeline combinators


type alias ValidationDecoder a =
    Dict String FieldValidation -> a


{-| Start a validation decoder pipeline.
-}
succeed : a -> ValidationDecoder a
succeed a _ =
    a


{-| Extract a FieldValidation for the given field name.
Returns `Just fieldValidation` if the field exists, `Nothing` otherwise.
-}
field : String -> ValidationDecoder (Maybe FieldValidation -> b) -> ValidationDecoder b
field key prev dict =
    prev dict (Dict.get key dict)



-- Helpers


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
                    case Decode.decodeValue Decode.string value of
                        Ok s ->
                            String.toFloat s

                        Err _ ->
                            Nothing


getIntArg : String -> Dict String Value -> Maybe Int
getIntArg key args =
    case Dict.get key args of
        Just val ->
            case Decode.decodeValue Decode.int val of
                Ok n ->
                    Just n

                Err _ ->
                    case Decode.decodeValue Decode.float val of
                        Ok f ->
                            Just (round f)

                        Err _ ->
                            Nothing

        Nothing ->
            Nothing


getNumberArg : String -> Dict String Value -> Maybe Float
getNumberArg key args =
    case Dict.get key args of
        Just val ->
            case Decode.decodeValue Decode.int val of
                Ok n ->
                    Just (toFloat n)

                Err _ ->
                    case Decode.decodeValue Decode.float val of
                        Ok f ->
                            Just f

                        Err _ ->
                            Nothing

        Nothing ->
            Nothing


getStringArg : String -> Dict String Value -> Maybe String
getStringArg key args =
    case Dict.get key args of
        Just val ->
            case Decode.decodeValue Decode.string val of
                Ok s ->
                    Just s

                Err _ ->
                    Nothing

        Nothing ->
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
                                            True
