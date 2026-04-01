module ValidationTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Internal.Condition as Condition
import JsonRender.Internal.PropValue exposing (PropValue(..))
import JsonRender.Validation as Validation
    exposing
        ( BuiltInCheck(..)
        , CheckType(..)
        , FieldValidation
        , ValidateOn(..)
        )
import Test exposing (..)


type alias TestValidations =
    { name : Maybe FieldValidation
    , email : Maybe FieldValidation
    }


suite : Test
suite =
    describe "JsonRender.Validation"
        [ checkTypeDecoderSuite
        , validateOnDecoderSuite
        , checkDecoderSuite
        , runCheckSuite
        , runValidationSuite
        , pipelineSuite
        , extractValidationSuite
        ]



-- CheckType decoder


checkTypeDecoderSuite : Test
checkTypeDecoderSuite =
    describe "checkTypeDecoder"
        [ test "decodes 'required'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"required\""
                    |> Expect.equal (Ok (BuiltIn Required))
        , test "decodes 'email'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"email\""
                    |> Expect.equal (Ok (BuiltIn Email))
        , test "decodes 'minLength'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"minLength\""
                    |> Expect.equal (Ok (BuiltIn MinLength))
        , test "decodes 'maxLength'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"maxLength\""
                    |> Expect.equal (Ok (BuiltIn MaxLength))
        , test "decodes 'pattern'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"pattern\""
                    |> Expect.equal (Ok (BuiltIn Pattern))
        , test "decodes 'min'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"min\""
                    |> Expect.equal (Ok (BuiltIn Min))
        , test "decodes 'max'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"max\""
                    |> Expect.equal (Ok (BuiltIn Max))
        , test "decodes 'numeric'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"numeric\""
                    |> Expect.equal (Ok (BuiltIn Numeric))
        , test "decodes 'url'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"url\""
                    |> Expect.equal (Ok (BuiltIn Url))
        , test "decodes 'matches'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"matches\""
                    |> Expect.equal (Ok (BuiltIn Matches))
        , test "decodes 'equalTo'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"equalTo\""
                    |> Expect.equal (Ok (BuiltIn EqualTo))
        , test "decodes 'lessThan'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"lessThan\""
                    |> Expect.equal (Ok (BuiltIn LessThan))
        , test "decodes 'greaterThan'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"greaterThan\""
                    |> Expect.equal (Ok (BuiltIn GreaterThan))
        , test "decodes 'requiredIf'" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"requiredIf\""
                    |> Expect.equal (Ok (BuiltIn RequiredIf))
        , test "decodes unknown string as Custom" <|
            \_ ->
                Decode.decodeString Validation.checkTypeDecoder "\"myCustomCheck\""
                    |> Expect.equal (Ok (Custom "myCustomCheck"))
        ]



-- ValidateOn decoder


validateOnDecoderSuite : Test
validateOnDecoderSuite =
    describe "validateOnDecoder"
        [ test "decodes 'change'" <|
            \_ ->
                Decode.decodeString Validation.validateOnDecoder "\"change\""
                    |> Expect.equal (Ok OnChange)
        , test "decodes 'blur'" <|
            \_ ->
                Decode.decodeString Validation.validateOnDecoder "\"blur\""
                    |> Expect.equal (Ok OnBlur)
        , test "decodes 'submit'" <|
            \_ ->
                Decode.decodeString Validation.validateOnDecoder "\"submit\""
                    |> Expect.equal (Ok OnSubmit)
        ]



-- ValidationCheck decoder


checkDecoderSuite : Test
checkDecoderSuite =
    describe "checkDecoder"
        [ test "decodes check with type and message" <|
            \_ ->
                let
                    json =
                        """{"type": "required", "message": "This field is required"}"""
                in
                case Decode.decodeString Validation.checkDecoder json of
                    Ok check ->
                        Expect.all
                            [ \c -> Expect.equal (BuiltIn Required) c.type_
                            , \c -> Expect.equal "This field is required" c.message
                            , \c -> Expect.equal Dict.empty c.args
                            ]
                            check

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes check with args" <|
            \_ ->
                let
                    json =
                        """{"type": "minLength", "message": "Too short", "args": {"min": 3}}"""
                in
                case Decode.decodeString Validation.checkDecoder json of
                    Ok check ->
                        Expect.all
                            [ \c -> Expect.equal (BuiltIn MinLength) c.type_
                            , \c -> Expect.equal "Too short" c.message
                            , \c -> Expect.equal (Just (IntValue 3)) (Dict.get "min" c.args)
                            ]
                            check

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "decodes check with state expression in args" <|
            \_ ->
                let
                    json =
                        """{"type": "matches", "message": "Must match", "args": {"other": {"$state": "/password"}}}"""
                in
                case Decode.decodeString Validation.checkDecoder json of
                    Ok check ->
                        Expect.all
                            [ \c -> Expect.equal (BuiltIn Matches) c.type_
                            , \c -> Expect.equal (Just (StateExpr "/password")) (Dict.get "other" c.args)
                            ]
                            check

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]



-- runCheck tests


runCheckSuite : Test
runCheckSuite =
    describe "runCheck"
        [ describe "required"
            [ test "passes for non-empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.string "hello") Dict.empty
                        |> Expect.equal True
            , test "fails for empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.string "") Dict.empty
                        |> Expect.equal False
            , test "fails for whitespace-only string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.string "  ") Dict.empty
                        |> Expect.equal False
            , test "fails for null" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) Encode.null Dict.empty
                        |> Expect.equal False
            , test "passes for non-empty list" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.list Encode.int [ 1 ]) Dict.empty
                        |> Expect.equal True
            , test "fails for empty list" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.list Encode.int []) Dict.empty
                        |> Expect.equal False
            ]
        , describe "email"
            [ test "passes for valid email" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "test@example.com") Dict.empty
                        |> Expect.equal True
            , test "fails for missing @" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "notanemail") Dict.empty
                        |> Expect.equal False
            , test "passes for empty string (not required)" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "") Dict.empty
                        |> Expect.equal True
            ]
        , describe "minLength"
            [ test "passes when string is long enough" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "hello")
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal True
            , test "fails when string is too short" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "hi")
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal False
            ]
        , describe "maxLength"
            [ test "passes when string is short enough" <|
                \_ ->
                    Validation.runCheck (BuiltIn MaxLength)
                        (Encode.string "hi")
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when string is too long" <|
                \_ ->
                    Validation.runCheck (BuiltIn MaxLength)
                        (Encode.string "hello world")
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal False
            ]
        , describe "pattern"
            [ test "passes when string matches regex" <|
                \_ ->
                    Validation.runCheck (BuiltIn Pattern)
                        (Encode.string "abc123")
                        (Dict.fromList [ ( "pattern", Encode.string "^[a-z]+[0-9]+$" ) ])
                        |> Expect.equal True
            , test "fails when string does not match regex" <|
                \_ ->
                    Validation.runCheck (BuiltIn Pattern)
                        (Encode.string "123abc")
                        (Dict.fromList [ ( "pattern", Encode.string "^[a-z]+[0-9]+$" ) ])
                        |> Expect.equal False
            ]
        , describe "min"
            [ test "passes when number >= min" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.int 10)
                        (Dict.fromList [ ( "min", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when number < min" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.int 3)
                        (Dict.fromList [ ( "min", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "passes for float >= min" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.float 5.5)
                        (Dict.fromList [ ( "min", Encode.float 5.0 ) ])
                        |> Expect.equal True
            ]
        , describe "max"
            [ test "passes when number <= max" <|
                \_ ->
                    Validation.runCheck (BuiltIn Max)
                        (Encode.int 3)
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when number > max" <|
                \_ ->
                    Validation.runCheck (BuiltIn Max)
                        (Encode.int 10)
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal False
            ]
        , describe "numeric"
            [ test "passes for integer" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.int 42) Dict.empty
                        |> Expect.equal True
            , test "passes for float" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.float 3.14) Dict.empty
                        |> Expect.equal True
            , test "passes for numeric string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.string "42.5") Dict.empty
                        |> Expect.equal True
            , test "fails for non-numeric string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.string "abc") Dict.empty
                        |> Expect.equal False
            , test "passes for empty string (not required)" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.string "") Dict.empty
                        |> Expect.equal True
            ]
        , describe "url"
            [ test "passes for http url" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "http://example.com") Dict.empty
                        |> Expect.equal True
            , test "passes for https url" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "https://example.com") Dict.empty
                        |> Expect.equal True
            , test "fails for non-url string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "not a url") Dict.empty
                        |> Expect.equal False
            , test "passes for empty string (not required)" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "") Dict.empty
                        |> Expect.equal True
            ]
        , describe "matches"
            [ test "passes when values are equal" <|
                \_ ->
                    Validation.runCheck (BuiltIn Matches)
                        (Encode.string "hello")
                        (Dict.fromList [ ( "other", Encode.string "hello" ) ])
                        |> Expect.equal True
            , test "fails when values differ" <|
                \_ ->
                    Validation.runCheck (BuiltIn Matches)
                        (Encode.string "hello")
                        (Dict.fromList [ ( "other", Encode.string "world" ) ])
                        |> Expect.equal False
            ]
        , describe "equalTo"
            [ test "passes when values are equal" <|
                \_ ->
                    Validation.runCheck (BuiltIn EqualTo)
                        (Encode.int 5)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when values differ" <|
                \_ ->
                    Validation.runCheck (BuiltIn EqualTo)
                        (Encode.int 5)
                        (Dict.fromList [ ( "other", Encode.int 10 ) ])
                        |> Expect.equal False
            ]
        , describe "lessThan"
            [ test "passes when value < arg" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.int 3)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when value >= arg" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.int 5)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            ]
        , describe "greaterThan"
            [ test "passes when value > arg" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.int 10)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when value <= arg" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.int 5)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            ]
        , describe "requiredIf"
            [ test "enforces required when field arg is truthy" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "")
                        (Dict.fromList [ ( "field", Encode.bool True ) ])
                        |> Expect.equal False
            , test "passes when field arg is falsy" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "")
                        (Dict.fromList [ ( "field", Encode.bool False ) ])
                        |> Expect.equal True
            , test "passes when field arg is truthy and value non-empty" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "hello")
                        (Dict.fromList [ ( "field", Encode.bool True ) ])
                        |> Expect.equal True
            ]
        , describe "Custom"
            [ test "unknown custom check passes by default" <|
                \_ ->
                    Validation.runCheck (Custom "unknownCheck") (Encode.string "anything") Dict.empty
                        |> Expect.equal True
            ]
        ]



-- runValidation tests


runValidationSuite : Test
runValidationSuite =
    describe "runValidation"
        [ test "all checks pass returns valid" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required" }
                            , { type_ = BuiltIn Email, args = Dict.empty, message = "Invalid email" }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "test@example.com") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal True r.valid
                    , \r -> Expect.equal [] r.errors
                    ]
                    result
        , test "failing checks returns errors" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required" }
                            , { type_ = BuiltIn Email, args = Dict.empty, message = "Invalid email" }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "notanemail") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal False r.valid
                    , \r -> Expect.equal [ "Invalid email" ] r.errors
                    ]
                    result
        , test "disabled config returns valid regardless" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required" }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Just (Condition.Or [])
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal True r.valid
                    , \r -> Expect.equal [] r.errors
                    ]
                    result
        , test "custom validation function is invoked" <|
            \_ ->
                let
                    customFn _ _ =
                        False

                    validationFns =
                        Dict.fromList [ ( "alwaysFail", customFn ) ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required" }
                            , { type_ = Custom "alwaysFail", args = Dict.empty, message = "Always fails" }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation validationFns Dict.empty config (Encode.string "hello") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal False r.valid
                    , \r -> Expect.equal [ "Always fails" ] r.errors
                    ]
                    result
        , test "empty value fails both required and minLength" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required" }
                            , { type_ = BuiltIn MinLength
                              , args = Dict.fromList [ ( "min", IntValue 3 ) ]
                              , message = "Too short"
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal False r.valid
                    , \r -> Expect.equal [ "Required", "Too short" ] r.errors
                    ]
                    result
        ]



-- Pipeline combinator tests


pipelineSuite : Test
pipelineSuite =
    describe "pipeline combinators"
        [ test "succeed returns constant value" <|
            \_ ->
                let
                    decoder =
                        Validation.succeed ()

                    result =
                        decoder Dict.empty
                in
                Expect.equal () result
        , test "field extracts FieldValidation when present" <|
            \_ ->
                let
                    fv =
                        { errors = [ "Required" ], touched = True, validated = True }

                    dict =
                        Dict.fromList [ ( "email", fv ) ]

                    decoder =
                        Validation.succeed identity
                            |> Validation.field "email"

                    result =
                        decoder dict
                in
                Expect.equal (Just fv) result
        , test "field returns Nothing when key absent" <|
            \_ ->
                let
                    decoder =
                        Validation.succeed identity
                            |> Validation.field "email"

                    result =
                        decoder Dict.empty
                in
                Expect.equal Nothing result
        , test "pipeline builds full record" <|
            \_ ->
                let
                    fv =
                        { errors = [], touched = True, validated = True }

                    dict =
                        Dict.fromList [ ( "name", fv ) ]

                    decoder =
                        Validation.succeed TestValidations
                            |> Validation.field "name"
                            |> Validation.field "email"

                    result =
                        decoder dict
                in
                Expect.all
                    [ \r -> Expect.equal (Just fv) r.name
                    , \r -> Expect.equal Nothing r.email
                    ]
                    result
        ]



-- extractValidation tests


extractValidationSuite : Test
extractValidationSuite =
    describe "extractValidation"
        [ test "extracts validation config from props with bindState and checks" <|
            \_ ->
                let
                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/email" )
                            , ( "checks"
                              , ListValue
                                    [ ObjectValue
                                        (Dict.fromList
                                            [ ( "type", StringValue "required" )
                                            , ( "message", StringValue "Email is required" )
                                            ]
                                        )
                                    , ObjectValue
                                        (Dict.fromList
                                            [ ( "type", StringValue "email" )
                                            , ( "message", StringValue "Invalid email" )
                                            ]
                                        )
                                    ]
                              )
                            ]
                in
                case Validation.extractValidation Nothing props of
                    Just ( path, config ) ->
                        Expect.all
                            [ \_ -> Expect.equal "/email" path
                            , \_ -> Expect.equal 2 (List.length config.checks)
                            , \_ -> Expect.equal OnSubmit config.validateOn
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected Just, got Nothing"
        , test "returns Nothing when no checks prop" <|
            \_ ->
                let
                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/email" ) ]
                in
                Validation.extractValidation Nothing props
                    |> Expect.equal Nothing
        , test "returns Nothing when no bindState prop" <|
            \_ ->
                let
                    props =
                        Dict.fromList
                            [ ( "value", StringValue "hello" )
                            , ( "checks"
                              , ListValue
                                    [ ObjectValue
                                        (Dict.fromList
                                            [ ( "type", StringValue "required" )
                                            , ( "message", StringValue "Required" )
                                            ]
                                        )
                                    ]
                              )
                            ]
                in
                Validation.extractValidation Nothing props
                    |> Expect.equal Nothing
        , test "extracts validateOn from props" <|
            \_ ->
                let
                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/name" )
                            , ( "checks"
                              , ListValue
                                    [ ObjectValue
                                        (Dict.fromList
                                            [ ( "type", StringValue "required" )
                                            , ( "message", StringValue "Required" )
                                            ]
                                        )
                                    ]
                              )
                            , ( "validateOn", StringValue "blur" )
                            ]
                in
                case Validation.extractValidation Nothing props of
                    Just ( _, config ) ->
                        Expect.equal OnBlur config.validateOn

                    Nothing ->
                        Expect.fail "Expected Just, got Nothing"
        , test "passes enabled condition through" <|
            \_ ->
                let
                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/name" )
                            , ( "checks"
                              , ListValue
                                    [ ObjectValue
                                        (Dict.fromList
                                            [ ( "type", StringValue "required" )
                                            , ( "message", StringValue "Required" )
                                            ]
                                        )
                                    ]
                              )
                            ]

                    enabledCond =
                        Just (Condition.And [])
                in
                case Validation.extractValidation enabledCond props of
                    Just ( _, config ) ->
                        Expect.equal enabledCond config.enabled

                    Nothing ->
                        Expect.fail "Expected Just, got Nothing"
        ]
