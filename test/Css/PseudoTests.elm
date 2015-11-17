module Css.PseudoTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css exposing (renderCompact, (:|))
import Css.Box exposing (borderStyle)
import Css.ColorsAndStrokes exposing (solid)
import Css.Elements exposing (p)

import Css.Pseudo exposing (..)

suite : Spec
suite = describe "Css.PseudoTests"
  [ describe "The pseudo classes"
    [ it "should render correctly"
        [ let stylesheet =
            [ (p :| lastOfType) [ borderStyle solid ] [] ]
          in renderCompact stylesheet 
              `shouldEqual` "p:last-of-type {border-style:solid}"
        ]
    ]
  , describe "The pseudo functions"
    [ it "should render correctly"
        [ let stylesheet =
            [ (p :| lang "no") [ borderStyle solid ] [] ]
          in renderCompact stylesheet 
              `shouldEqual` "p:lang(no) {border-style:solid}"
        ]
    ]
  ]
