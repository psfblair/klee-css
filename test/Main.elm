module Main where

import IO.IO exposing (..)
import IO.Runner exposing (Request, Response, run)

import Spec.Runner.Console as Console
import Spec exposing (..)

import Css.PropertyTests

allTests : Spec
allTests =
  describe  "All tests"
            Css.PropertyTests.suite

testRunner : IO ()
testRunner = Console.run allTests
             
port requests : Signal Request
port requests = run responses testRunner
                             
port responses : Signal Response
                                              
