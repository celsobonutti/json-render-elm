module Test.Generated.Main exposing (main)

import ActionsTest
import RenderTest
import ResolveTest
import SpecTest
import StateTest
import VisibilityTest

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport Monochrome
        , seed = 220720127473768
        , processes = 10
        , globs =
            []
        , paths =
            [ "/Users/dokkora/work/json-render-elm/packages/elm-core/tests/ActionsTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-core/tests/RenderTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-core/tests/ResolveTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-core/tests/SpecTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-core/tests/StateTest.elm"
            , "/Users/dokkora/work/json-render-elm/packages/elm-core/tests/VisibilityTest.elm"
            ]
        }
        [ ( "ActionsTest"
          , [ Test.Runner.Node.check ActionsTest.suite
            ]
          )
        , ( "RenderTest"
          , [ Test.Runner.Node.check RenderTest.testRegistry
            , Test.Runner.Node.check RenderTest.suite
            ]
          )
        , ( "ResolveTest"
          , [ Test.Runner.Node.check ResolveTest.state
            , Test.Runner.Node.check ResolveTest.suite
            ]
          )
        , ( "SpecTest"
          , [ Test.Runner.Node.check SpecTest.specJson
            , Test.Runner.Node.check SpecTest.suite
            ]
          )
        , ( "StateTest"
          , [ Test.Runner.Node.check StateTest.suite
            ]
          )
        , ( "VisibilityTest"
          , [ Test.Runner.Node.check VisibilityTest.state
            , Test.Runner.Node.check VisibilityTest.suite
            ]
          )
        ]