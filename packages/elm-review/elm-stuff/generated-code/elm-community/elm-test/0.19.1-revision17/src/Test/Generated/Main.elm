module Test.Generated.Main exposing (main)

import CatalogSyncTest
import ElmCodeGenTest
import SchemaParserTest
import TypeMappingTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport Monochrome
        , seed = 190707488936299
        , processes = 10
        , globs =
            []
        , paths =
            [ "/Users/dokkora/work/json-render-elm/packages/elm-review/tests/CatalogSyncTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-review/tests/ElmCodeGenTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-review/tests/SchemaParserTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-review/tests/TypeMappingTest.elm"
            ]
        }
        [ ( "CatalogSyncTest"
          , [ Test.Runner.Node.check CatalogSyncTest.sampleSchemaJson
            , Test.Runner.Node.check CatalogSyncTest.baseConfig
            , Test.Runner.Node.check CatalogSyncTest.suite
            ]
          )
        , ( "ElmCodeGenTest"
          , [ Test.Runner.Node.check ElmCodeGenTest.cardSchema
            , Test.Runner.Node.check ElmCodeGenTest.suite
            ]
          )
        , ( "SchemaParserTest"
          , [ Test.Runner.Node.check SchemaParserTest.sampleSchema
            , Test.Runner.Node.check SchemaParserTest.suite
            ]
          )
        , ( "TypeMappingTest"
          , [ Test.Runner.Node.check TypeMappingTest.suite
            ]
          )
        ]