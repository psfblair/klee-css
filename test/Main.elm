module Main where

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)

import Spec.Runner.Console as Console
import Spec exposing (..)

import CssTests
import Css.PropertyTests
import Css.ColorTests

allTests : Spec
allTests =
  describe  "All tests" 
    [ CssTests.suite
    , Css.PropertyTests.suite
    , Css.ColorTests.suite
    ]

testRunner : IO ()
testRunner = Console.run allTests

port requests : Signal Request
port requests = run responses testRunner

port responses : Signal Response
