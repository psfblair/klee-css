module Main where

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)

import Spec.Runner.Console as Console
import Spec exposing (..)

import CssTests
import Css.BackgroundTests
import Css.BoxTests
import Css.ColorTests
import Css.GeometryTests
import Css.LayoutTests
import Css.PointerTests
import Css.PseudoTests
import Css.TypographyTests
import Css.Internal.InternalColorTests
import Css.Internal.PropertyTests
import Css.Internal.UtilsTests

-------------------------------------------------------------------------------

allTests : Spec
allTests =
  describe  "All tests"
    [ CssTests.suite
    , Css.BackgroundTests.suite
    , Css.BoxTests.suite
    , Css.ColorTests.suite
    , Css.GeometryTests.suite
    , Css.LayoutTests.suite
    , Css.PointerTests.suite
    , Css.PseudoTests.suite
    , Css.TypographyTests.suite
    , Css.Internal.InternalColorTests.suite
    , Css.Internal.PropertyTests.suite
    , Css.Internal.UtilsTests.suite
    ]

testRunner : IO ()
testRunner = Console.run allTests

port requests : Signal Request
port requests = run responses testRunner

port responses : Signal Response
