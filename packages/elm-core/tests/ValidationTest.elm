module ValidationTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRender.Internal.Condition as Condition
import JsonRender.Internal.PropValue exposing (ConditionExpr(..), PropValue(..))
import JsonRender.Resolve as Resolve
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
        , runValidationCheckSuite
        , deepArgResolutionSuite
        , pipelineSuite
        , extractValidationSuite
        , configRoundTripSuite
        , complexExprArgsSuite
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



-- runCheck tests (matches builtInValidationFunctions in core)


runCheckSuite : Test
runCheckSuite =
    describe "runCheck"
        [ describe "required"
            [ test "passes for non-empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.string "hello") Dict.empty
                        |> Expect.equal True
            , test "passes for zero" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.int 0) Dict.empty
                        |> Expect.equal True
            , test "passes for false" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.bool False) Dict.empty
                        |> Expect.equal True
            , test "passes for non-empty list" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.list Encode.string [ "item" ]) Dict.empty
                        |> Expect.equal True
            , test "passes for object" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.object [ ( "key", Encode.string "value" ) ]) Dict.empty
                        |> Expect.equal True
            , test "fails for empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.string "") Dict.empty
                        |> Expect.equal False
            , test "fails for whitespace-only string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) (Encode.string "   ") Dict.empty
                        |> Expect.equal False
            , test "fails for null" <|
                \_ ->
                    Validation.runCheck (BuiltIn Required) Encode.null Dict.empty
                        |> Expect.equal False
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
            , test "passes for email with dots in username" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "user.name@domain.co") Dict.empty
                        |> Expect.equal True
            , test "passes for minimal email" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "a@b.c") Dict.empty
                        |> Expect.equal True
            , test "fails for missing domain extension" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "missing@domain") Dict.empty
                        |> Expect.equal False
            , test "fails for missing username" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "@domain.com") Dict.empty
                        |> Expect.equal False
            , test "fails for missing domain" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "user@") Dict.empty
                        |> Expect.equal False
            , test "fails for plain string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "invalid") Dict.empty
                        |> Expect.equal False
            , test "fails for non-string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.int 123) Dict.empty
                        |> Expect.equal False
            , test "fails for empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Email) (Encode.string "") Dict.empty
                        |> Expect.equal False
            ]
        , describe "minLength"
            [ test "passes when string meets minimum length" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "hello")
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal True
            , test "passes when string is exactly minimum length" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "abc")
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal True
            , test "passes when string exceeds minimum length" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "abcdef")
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal True
            , test "fails when string is too short" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "hi")
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal False
            , test "fails for empty string with min 1" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "")
                        (Dict.fromList [ ( "min", Encode.int 1 ) ])
                        |> Expect.equal False
            , test "fails for non-strings" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.int 123)
                        (Dict.fromList [ ( "min", Encode.int 1 ) ])
                        |> Expect.equal False
            , test "fails when min is not provided" <|
                \_ ->
                    Validation.runCheck (BuiltIn MinLength)
                        (Encode.string "hello")
                        Dict.empty
                        |> Expect.equal False
            ]
        , describe "maxLength"
            [ test "passes when string is under maximum" <|
                \_ ->
                    Validation.runCheck (BuiltIn MaxLength)
                        (Encode.string "hi")
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "passes when string is exactly maximum" <|
                \_ ->
                    Validation.runCheck (BuiltIn MaxLength)
                        (Encode.string "hello")
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when string exceeds maximum" <|
                \_ ->
                    Validation.runCheck (BuiltIn MaxLength)
                        (Encode.string "hello!")
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal False
            ]
        , describe "pattern"
            [ test "passes when string matches pattern" <|
                \_ ->
                    Validation.runCheck (BuiltIn Pattern)
                        (Encode.string "abc123")
                        (Dict.fromList [ ( "pattern", Encode.string "^[a-z0-9]+$" ) ])
                        |> Expect.equal True
            , test "fails when string does not match pattern" <|
                \_ ->
                    Validation.runCheck (BuiltIn Pattern)
                        (Encode.string "ABC")
                        (Dict.fromList [ ( "pattern", Encode.string "^[a-z]+$" ) ])
                        |> Expect.equal False
            , test "fails for invalid regex pattern" <|
                \_ ->
                    Validation.runCheck (BuiltIn Pattern)
                        (Encode.string "test")
                        (Dict.fromList [ ( "pattern", Encode.string "[invalid" ) ])
                        |> Expect.equal False
            ]
        , describe "min"
            [ test "passes when number exceeds minimum" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.int 5)
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal True
            , test "passes when number equals minimum" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.int 3)
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal True
            , test "fails when number is below minimum" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.int 2)
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal False
            , test "fails for non-numbers" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.string "5")
                        (Dict.fromList [ ( "min", Encode.int 3 ) ])
                        |> Expect.equal False
            , test "passes for float >= min" <|
                \_ ->
                    Validation.runCheck (BuiltIn Min)
                        (Encode.float 5.5)
                        (Dict.fromList [ ( "min", Encode.float 5.0 ) ])
                        |> Expect.equal True
            ]
        , describe "max"
            [ test "passes when number is under maximum" <|
                \_ ->
                    Validation.runCheck (BuiltIn Max)
                        (Encode.int 3)
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "passes when number equals maximum" <|
                \_ ->
                    Validation.runCheck (BuiltIn Max)
                        (Encode.int 5)
                        (Dict.fromList [ ( "max", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when number exceeds maximum" <|
                \_ ->
                    Validation.runCheck (BuiltIn Max)
                        (Encode.int 6)
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
            , test "passes for zero" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.int 0) Dict.empty
                        |> Expect.equal True
            , test "passes for numeric string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.string "42") Dict.empty
                        |> Expect.equal True
            , test "passes for float string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.string "3.14") Dict.empty
                        |> Expect.equal True
            , test "fails for non-numeric string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.string "abc") Dict.empty
                        |> Expect.equal False
            , test "fails for null" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) Encode.null Dict.empty
                        |> Expect.equal False
            , test "fails for empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Numeric) (Encode.string "") Dict.empty
                        |> Expect.equal False
            ]
        , describe "url"
            [ test "passes for https url" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "https://example.com") Dict.empty
                        |> Expect.equal True
            , test "passes for http localhost with port" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "http://localhost:3000") Dict.empty
                        |> Expect.equal True
            , test "passes for url with path and query" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "https://example.com/path?query=1") Dict.empty
                        |> Expect.equal True
            , test "fails for string without protocol" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "example.com") Dict.empty
                        |> Expect.equal False
            , test "fails for non-url string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "not-a-url") Dict.empty
                        |> Expect.equal False
            , test "fails for empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn Url) (Encode.string "") Dict.empty
                        |> Expect.equal False
            ]
        , describe "matches"
            [ test "passes when string values match" <|
                \_ ->
                    Validation.runCheck (BuiltIn Matches)
                        (Encode.string "password")
                        (Dict.fromList [ ( "other", Encode.string "password" ) ])
                        |> Expect.equal True
            , test "passes when number values match" <|
                \_ ->
                    Validation.runCheck (BuiltIn Matches)
                        (Encode.int 123)
                        (Dict.fromList [ ( "other", Encode.int 123 ) ])
                        |> Expect.equal True
            , test "fails when values do not match" <|
                \_ ->
                    Validation.runCheck (BuiltIn Matches)
                        (Encode.string "password")
                        (Dict.fromList [ ( "other", Encode.string "different" ) ])
                        |> Expect.equal False
            ]
        , describe "equalTo"
            [ test "passes when values are equal" <|
                \_ ->
                    Validation.runCheck (BuiltIn EqualTo)
                        (Encode.string "abc")
                        (Dict.fromList [ ( "other", Encode.string "abc" ) ])
                        |> Expect.equal True
            , test "fails when values differ" <|
                \_ ->
                    Validation.runCheck (BuiltIn EqualTo)
                        (Encode.string "abc")
                        (Dict.fromList [ ( "other", Encode.string "xyz" ) ])
                        |> Expect.equal False
            ]
        , describe "lessThan"
            [ test "passes when value is less than other" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.int 3)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when value equals other" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.int 5)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "fails when value is greater than other" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.int 7)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "coerces numeric string vs number" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.string "3")
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails coercion when non-numeric string" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.string "abc")
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "passes for string comparison (ISO dates)" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.string "2026-01-01")
                        (Dict.fromList [ ( "other", Encode.string "2026-06-15" ) ])
                        |> Expect.equal True
            , test "fails for equal strings" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.string "2026-01-01")
                        (Dict.fromList [ ( "other", Encode.string "2026-01-01" ) ])
                        |> Expect.equal False
            , test "returns false when value is empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.string "")
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "returns false when other is empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.int 3)
                        (Dict.fromList [ ( "other", Encode.string "" ) ])
                        |> Expect.equal False
            , test "returns false when value is empty string vs non-empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.string "")
                        (Dict.fromList [ ( "other", Encode.string "abc" ) ])
                        |> Expect.equal False
            , test "returns false when other is empty string vs non-empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.string "abc")
                        (Dict.fromList [ ( "other", Encode.string "" ) ])
                        |> Expect.equal False
            , test "returns false when other is null" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        (Encode.int 3)
                        (Dict.fromList [ ( "other", Encode.null ) ])
                        |> Expect.equal False
            , test "returns false when value is null" <|
                \_ ->
                    Validation.runCheck (BuiltIn LessThan)
                        Encode.null
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            ]
        , describe "greaterThan"
            [ test "passes when value is greater than other" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.int 7)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails when value equals other" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.int 5)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "fails when value is less than other" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.int 3)
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "coerces numeric string vs number" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.string "7")
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal True
            , test "fails coercion when non-numeric string" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.string "abc")
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "passes for string comparison (ISO dates)" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.string "2026-06-15")
                        (Dict.fromList [ ( "other", Encode.string "2026-01-01" ) ])
                        |> Expect.equal True
            , test "fails for lesser strings" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.string "2026-01-01")
                        (Dict.fromList [ ( "other", Encode.string "2026-06-15" ) ])
                        |> Expect.equal False
            , test "returns false when value is empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.string "")
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            , test "returns false when other is empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.int 3)
                        (Dict.fromList [ ( "other", Encode.string "" ) ])
                        |> Expect.equal False
            , test "returns false when value is empty string vs non-empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.string "")
                        (Dict.fromList [ ( "other", Encode.string "abc" ) ])
                        |> Expect.equal False
            , test "returns false when other is empty string vs non-empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.string "abc")
                        (Dict.fromList [ ( "other", Encode.string "" ) ])
                        |> Expect.equal False
            , test "returns false when other is null" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        (Encode.int 3)
                        (Dict.fromList [ ( "other", Encode.null ) ])
                        |> Expect.equal False
            , test "returns false when value is null" <|
                \_ ->
                    Validation.runCheck (BuiltIn GreaterThan)
                        Encode.null
                        (Dict.fromList [ ( "other", Encode.int 5 ) ])
                        |> Expect.equal False
            ]
        , describe "requiredIf"
            [ test "passes when condition is falsy (field not required)" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "")
                        (Dict.fromList [ ( "field", Encode.bool False ) ])
                        |> Expect.equal True
            , test "passes when condition field is empty string" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "")
                        (Dict.fromList [ ( "field", Encode.string "" ) ])
                        |> Expect.equal True
            , test "passes when condition field is null" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "")
                        (Dict.fromList [ ( "field", Encode.null ) ])
                        |> Expect.equal True
            , test "fails when condition is truthy and value is empty" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "")
                        (Dict.fromList [ ( "field", Encode.bool True ) ])
                        |> Expect.equal False
            , test "fails when condition is truthy string and value is null" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        Encode.null
                        (Dict.fromList [ ( "field", Encode.string "yes" ) ])
                        |> Expect.equal False
            , test "passes when condition is truthy and value is present" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.string "hello")
                        (Dict.fromList [ ( "field", Encode.bool True ) ])
                        |> Expect.equal True
            , test "passes when condition is truthy and value is number" <|
                \_ ->
                    Validation.runCheck (BuiltIn RequiredIf)
                        (Encode.int 42)
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



-- runValidationCheck tests (matches runValidationCheck in core)


runValidationCheckSuite : Test
runValidationCheckSuite =
    describe "runValidationCheck"
        [ test "runs a validation check and returns result" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null } ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "hello") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal True r.valid
                    , \r -> Expect.equal [] r.errors
                    ]
                    result
        , test "resolves dynamic args from state" <|
            \_ ->
                let
                    state =
                        Encode.object [ ( "minLen", Encode.int 5 ) ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn MinLength
                              , args = Dict.fromList [ ( "min", StateExpr "/minLen" ) ]
                              , message = "Too short"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "hi") state Nothing
                in
                Expect.equal False result.valid
        , test "returns valid for unknown custom functions" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = Custom "unknownFunction", args = Dict.empty, message = "Unknown", raw = Encode.null } ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "test") Encode.null Nothing
                in
                Expect.equal True result.valid
        , test "uses custom validation functions" <|
            \_ ->
                let
                    startsWithA value _ =
                        case Decode.decodeValue Decode.string value of
                            Ok s ->
                                String.startsWith "A" s

                            Err _ ->
                                False

                    customFns =
                        Dict.fromList [ ( "startsWithA", startsWithA ) ]

                    config =
                        { checks =
                            [ { type_ = Custom "startsWithA", args = Dict.empty, message = "Must start with A", raw = Encode.null } ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation customFns Dict.empty config (Encode.string "Apple") Encode.null Nothing
                in
                Expect.equal True result.valid
        ]



-- runValidation tests (matches runValidation in core)


runValidationSuite : Test
runValidationSuite =
    describe "runValidation"
        [ test "runs all checks and returns valid" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null }
                            , { type_ = BuiltIn Email, args = Dict.empty, message = "Invalid email", raw = Encode.null }
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
        , test "collects all errors" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null }
                            , { type_ = BuiltIn Email, args = Dict.empty, message = "Invalid email", raw = Encode.null }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal False r.valid
                    , \r -> Expect.equal True (List.member "Required" r.errors)
                    , \r -> Expect.equal True (List.member "Invalid email" r.errors)
                    ]
                    result
        , test "skips validation when enabled condition is false" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null } ]
                        , validateOn = OnSubmit
                        , enabled = Just (Condition.Compare (Condition.StateSource "/enabled") (Condition.Eq (Condition.Literal (Encode.bool True))))
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "") Encode.null Nothing
                in
                Expect.all
                    [ \r -> Expect.equal True r.valid
                    , \r -> Expect.equal [] r.errors
                    ]
                    result
        , test "runs validation when enabled condition is true" <|
            \_ ->
                let
                    state =
                        Encode.object [ ( "enabled", Encode.bool True ) ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null } ]
                        , validateOn = OnSubmit
                        , enabled = Just (Condition.Compare (Condition.StateSource "/enabled") Condition.IsTruthy)
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "") state Nothing
                in
                Expect.equal False result.valid
        , test "custom validation function is invoked" <|
            \_ ->
                let
                    customFn _ _ =
                        False

                    validationFns =
                        Dict.fromList [ ( "alwaysFail", customFn ) ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null }
                            , { type_ = Custom "alwaysFail", args = Dict.empty, message = "Always fails", raw = Encode.null }
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
                            [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null }
                            , { type_ = BuiltIn MinLength
                              , args = Dict.fromList [ ( "min", IntValue 3 ) ]
                              , message = "Too short"
                              , raw = Encode.null
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



-- Deep arg resolution tests (matches deep arg resolution in core)


deepArgResolutionSuite : Test
deepArgResolutionSuite =
    describe "deep arg resolution"
        [ test "resolves nested $state refs in matches validation args" <|
            \_ ->
                let
                    state =
                        Encode.object
                            [ ( "form"
                              , Encode.object [ ( "password", Encode.string "secret123" ) ]
                              )
                            ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn Matches
                              , args = Dict.fromList [ ( "other", StateExpr "/form/password" ) ]
                              , message = "Passwords must match"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "secret123") state Nothing
                in
                Expect.equal True result.valid
        , test "resolves $state in cross-field lessThan check" <|
            \_ ->
                let
                    state =
                        Encode.object
                            [ ( "form"
                              , Encode.object [ ( "maxPrice", Encode.int 100 ) ]
                              )
                            ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn LessThan
                              , args = Dict.fromList [ ( "other", StateExpr "/form/maxPrice" ) ]
                              , message = "Must be less than max price"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.int 50) state Nothing
                in
                Expect.equal True result.valid
        , test "resolves $state in requiredIf check - condition true" <|
            \_ ->
                let
                    state =
                        Encode.object
                            [ ( "form"
                              , Encode.object [ ( "enableEmail", Encode.bool True ) ]
                              )
                            ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn RequiredIf
                              , args = Dict.fromList [ ( "field", StateExpr "/form/enableEmail" ) ]
                              , message = "Email is required"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "") state Nothing
                in
                Expect.equal False result.valid
        , test "passes requiredIf when $state condition is false" <|
            \_ ->
                let
                    state =
                        Encode.object
                            [ ( "form"
                              , Encode.object [ ( "enableEmail", Encode.bool False ) ]
                              )
                            ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn RequiredIf
                              , args = Dict.fromList [ ( "field", StateExpr "/form/enableEmail" ) ]
                              , message = "Email is required"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    result =
                        Validation.runValidation Dict.empty Dict.empty config (Encode.string "") state Nothing
                in
                Expect.equal True result.valid
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
        [ test "extracts validation config from element fields with bindState" <|
            \_ ->
                let
                    checks =
                        [ { type_ = BuiltIn Required, args = Dict.empty, message = "Email is required", raw = Encode.null }
                        , { type_ = BuiltIn Email, args = Dict.empty, message = "Invalid email", raw = Encode.null }
                        ]

                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/email" ) ]
                in
                case Validation.extractValidation checks OnSubmit Nothing props of
                    Just ( path, config ) ->
                        Expect.all
                            [ \_ -> Expect.equal "/email" path
                            , \_ -> Expect.equal 2 (List.length config.checks)
                            , \_ -> Expect.equal OnSubmit config.validateOn
                            ]
                            ()

                    Nothing ->
                        Expect.fail "Expected Just, got Nothing"
        , test "returns Nothing when checks list is empty" <|
            \_ ->
                let
                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/email" ) ]
                in
                Validation.extractValidation [] OnSubmit Nothing props
                    |> Expect.equal Nothing
        , test "returns Nothing when no bindState prop" <|
            \_ ->
                let
                    checks =
                        [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null } ]

                    props =
                        Dict.fromList
                            [ ( "value", StringValue "hello" ) ]
                in
                Validation.extractValidation checks OnSubmit Nothing props
                    |> Expect.equal Nothing
        , test "passes validateOn through" <|
            \_ ->
                let
                    checks =
                        [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null } ]

                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/name" ) ]
                in
                case Validation.extractValidation checks OnBlur Nothing props of
                    Just ( _, config ) ->
                        Expect.equal OnBlur config.validateOn

                    Nothing ->
                        Expect.fail "Expected Just, got Nothing"
        , test "passes enabled condition through" <|
            \_ ->
                let
                    checks =
                        [ { type_ = BuiltIn Required, args = Dict.empty, message = "Required", raw = Encode.null } ]

                    props =
                        Dict.fromList
                            [ ( "value", BindStateExpr "/name" ) ]

                    enabledCond =
                        Just (Condition.And [])
                in
                case Validation.extractValidation checks OnSubmit enabledCond props of
                    Just ( _, config ) ->
                        Expect.equal enabledCond config.enabled

                    Nothing ->
                        Expect.fail "Expected Just, got Nothing"
        ]



-- Config round-trip tests


configRoundTripSuite : Test
configRoundTripSuite =
    describe "config round-trip"
        [ test "encodeValidationConfig >> configDecoder round-trips check count and validateOn" <|
            \_ ->
                let
                    json =
                        """{"checks":[{"type":"required","message":"Field is required"},{"type":"minLength","args":{"min":3},"message":"Too short"}],"validateOn":"blur"}"""
                in
                case Decode.decodeString Validation.configDecoder json of
                    Ok ( config, maybeRepeatCtx ) ->
                        Expect.all
                            [ \_ -> Expect.equal 2 (List.length config.checks)
                            , \_ -> Expect.equal OnBlur config.validateOn
                            , \_ -> Expect.equal Nothing maybeRepeatCtx
                            , \_ ->
                                List.head config.checks
                                    |> Maybe.map .type_
                                    |> Expect.equal (Just (BuiltIn Required))
                            , \_ ->
                                List.drop 1 config.checks
                                    |> List.head
                                    |> Maybe.map .type_
                                    |> Expect.equal (Just (BuiltIn MinLength))
                            ]
                            ()

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "round-trips custom check type" <|
            \_ ->
                let
                    json =
                        """{"checks":[{"type":"myValidator","message":"Custom failed"}],"validateOn":"submit"}"""
                in
                case Decode.decodeString Validation.configDecoder json of
                    Ok ( config, maybeRepeatCtx ) ->
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length config.checks)
                            , \_ -> Expect.equal OnSubmit config.validateOn
                            , \_ -> Expect.equal Nothing maybeRepeatCtx
                            , \_ ->
                                List.head config.checks
                                    |> Maybe.map .type_
                                    |> Expect.equal (Just (Custom "myValidator"))
                            ]
                            ()

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "encodeValidationConfig with repeatCtx encodes and decodes repeatCtx" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required
                              , args = Dict.empty
                              , message = "Required"
                              , raw = Encode.object [ ( "type", Encode.string "required" ), ( "message", Encode.string "Required" ) ]
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    repeatCtx =
                        Just { item = Encode.object [ ( "name", Encode.string "Alice" ) ], index = 2, basePath = "/items" }

                    encoded =
                        Validation.encodeValidationConfig config repeatCtx

                    decoded =
                        Decode.decodeValue Validation.configDecoder encoded
                in
                case decoded of
                    Ok ( decodedConfig, decodedRepeatCtx ) ->
                        Expect.all
                            [ \_ -> Expect.equal 1 (List.length decodedConfig.checks)
                            , \_ -> Expect.equal OnSubmit decodedConfig.validateOn
                            , \_ ->
                                decodedRepeatCtx
                                    |> Maybe.map .index
                                    |> Expect.equal (Just 2)
                            , \_ ->
                                decodedRepeatCtx
                                    |> Maybe.map .basePath
                                    |> Expect.equal (Just "/items")
                            ]
                            ()

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "encodeValidationConfig without repeatCtx produces Nothing on decode" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Required
                              , args = Dict.empty
                              , message = "Required"
                              , raw = Encode.object [ ( "type", Encode.string "required" ), ( "message", Encode.string "Required" ) ]
                              }
                            ]
                        , validateOn = OnChange
                        , enabled = Nothing
                        }

                    encoded =
                        Validation.encodeValidationConfig config Nothing

                    decoded =
                        Decode.decodeValue Validation.configDecoder encoded
                in
                case decoded of
                    Ok ( _, maybeRepeatCtx ) ->
                        Expect.equal Nothing maybeRepeatCtx

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]



-- complex expression args tests


complexExprArgsSuite : Test
complexExprArgsSuite =
    describe "complex expression args"
        [ test "$computed in check args resolves function result" <|
            \_ ->
                let
                    functions =
                        Dict.fromList
                            [ ( "getMinLength"
                              , \args ->
                                    case Dict.get "base" args of
                                        Just (Resolve.RInt n) ->
                                            Resolve.RInt (n + 2)

                                        _ ->
                                            Resolve.RError "expected int"
                              )
                            ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn MinLength
                              , args = Dict.fromList
                                    [ ( "min", ComputedExpr "getMinLength" (Dict.fromList [ ( "base", IntValue 3 ) ]) ) ]
                              , message = "Too short"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    -- "ab" has length 2, minLength is 3+2=5, should fail
                    fieldValue =
                        Encode.string "ab"

                    state =
                        Encode.object []

                    result =
                        Validation.runValidation Dict.empty functions config fieldValue state Nothing
                in
                Expect.equal False result.valid
        , test "$computed in check args — passing when value meets computed min" <|
            \_ ->
                let
                    functions =
                        Dict.fromList
                            [ ( "getMinLength"
                              , \args ->
                                    case Dict.get "base" args of
                                        Just (Resolve.RInt n) ->
                                            Resolve.RInt (n + 2)

                                        _ ->
                                            Resolve.RError "expected int"
                              )
                            ]

                    config =
                        { checks =
                            [ { type_ = BuiltIn MinLength
                              , args = Dict.fromList
                                    [ ( "min", ComputedExpr "getMinLength" (Dict.fromList [ ( "base", IntValue 3 ) ]) ) ]
                              , message = "Too short"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    -- "hello" has length 5, minLength is 5, should pass
                    fieldValue =
                        Encode.string "hello"

                    state =
                        Encode.object []

                    result =
                        Validation.runValidation Dict.empty functions config fieldValue state Nothing
                in
                Expect.equal True result.valid
        , test "$cond in check args resolves conditional value" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Matches
                              , args =
                                    Dict.fromList
                                        [ ( "other"
                                          , ConditionalExpr
                                                (TruthyExpr (StateExpr "/useCustom"))
                                                (StateExpr "/customValue")
                                                (StringValue "default")
                                          )
                                        ]
                              , message = "Must match"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    -- /useCustom is false, so $cond resolves to "default"
                    state =
                        Encode.object
                            [ ( "useCustom", Encode.bool False )
                            , ( "customValue", Encode.string "custom" )
                            ]

                    fieldValue =
                        Encode.string "default"

                    result =
                        Validation.runValidation Dict.empty Dict.empty config fieldValue state Nothing
                in
                Expect.equal True result.valid
        , test "$cond in check args — fails when field doesn't match resolved branch" <|
            \_ ->
                let
                    config =
                        { checks =
                            [ { type_ = BuiltIn Matches
                              , args =
                                    Dict.fromList
                                        [ ( "other"
                                          , ConditionalExpr
                                                (TruthyExpr (StateExpr "/useCustom"))
                                                (StateExpr "/customValue")
                                                (StringValue "default")
                                          )
                                        ]
                              , message = "Must match"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    -- /useCustom is true, so $cond resolves to /customValue = "custom"
                    state =
                        Encode.object
                            [ ( "useCustom", Encode.bool True )
                            , ( "customValue", Encode.string "custom" )
                            ]

                    fieldValue =
                        Encode.string "default"

                    result =
                        Validation.runValidation Dict.empty Dict.empty config fieldValue state Nothing
                in
                Expect.equal False result.valid
        , test "$item in check args resolves to absolute state path string" <|
            \_ ->
                let
                    repeatCtx =
                        Just
                            { item = Encode.object []
                            , index = 0
                            , basePath = "/users/0"
                            }

                    config =
                        { checks =
                            [ { type_ = BuiltIn Matches
                              , args = Dict.fromList [ ( "other", ItemExpr "confirmPassword" ) ]
                              , message = "Must match path"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    -- $item "confirmPassword" in action context resolves to the absolute path string "/users/0/confirmPassword"
                    fieldValue =
                        Encode.string "/users/0/confirmPassword"

                    state =
                        Encode.object []

                    result =
                        Validation.runValidation Dict.empty Dict.empty config fieldValue state repeatCtx
                in
                Expect.equal True result.valid
        , test "$item in check args — fails when field value doesn't match resolved path" <|
            \_ ->
                let
                    repeatCtx =
                        Just
                            { item = Encode.object []
                            , index = 0
                            , basePath = "/users/0"
                            }

                    config =
                        { checks =
                            [ { type_ = BuiltIn Matches
                              , args = Dict.fromList [ ( "other", ItemExpr "confirmPassword" ) ]
                              , message = "Must match path"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    -- $item "confirmPassword" resolves to "/users/0/confirmPassword", but fieldValue is different
                    fieldValue =
                        Encode.string "secret123"

                    state =
                        Encode.object []

                    result =
                        Validation.runValidation Dict.empty Dict.empty config fieldValue state repeatCtx
                in
                Expect.equal False result.valid
        , test "$index in check args resolves to current index" <|
            \_ ->
                let
                    repeatCtx =
                        Just
                            { item = Encode.object []
                            , index = 5
                            , basePath = "/items/5"
                            }

                    config =
                        { checks =
                            [ { type_ = BuiltIn Min
                              , args = Dict.fromList [ ( "min", IndexExpr ) ]
                              , message = "Must be at least index value"
                              , raw = Encode.null
                              }
                            ]
                        , validateOn = OnSubmit
                        , enabled = Nothing
                        }

                    fieldValue =
                        Encode.int 3

                    state =
                        Encode.object []

                    result =
                        Validation.runValidation Dict.empty Dict.empty config fieldValue state repeatCtx
                in
                -- value 3 < index 5, so min check fails
                Expect.equal False result.valid
        ]
