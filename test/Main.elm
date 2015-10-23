module Main where

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)

import Spec.Runner.Console as Console
import Spec exposing (..)

import CssTests
import Css.Internal.PropertyTests
import Css.Internal.UtilsTests
import Css.BorderTests
import Css.BoxTests
import Css.ColorTests
import Css.FontTests
import Css.GeometryTests

-------------------------------------------------------------------------------

allTests : Spec
allTests =
  describe  "All tests"
    [ CssTests.suite
    , Css.Internal.PropertyTests.suite
    , Css.Internal.UtilsTests.suite
    , Css.BorderTests.suite
    , Css.BoxTests.suite
    , Css.ColorTests.suite
    , Css.FontTests.suite
    , Css.GeometryTests.suite
    ]

testRunner : IO ()
testRunner = Console.run allTests

port requests : Signal Request
port requests = run responses testRunner

port responses : Signal Response
