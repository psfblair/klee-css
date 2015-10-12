module Css.PseudoTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Pseudo exposing (..)
import Css.Elements exposing (p)
import Css.Border exposing (solid,borderStyle)
import Css exposing (renderCompact, (.|))

suite : Spec
suite = describe "Css.PseudoTests"
  [ describe "The pseudo classes"
    [ it "should render correctly"
        [ renderCompact ((p .| lastOfType) [ borderStyle solid ])
            `shouldEqual` "p:last-of-type{border-style:solid}"
        ]
    ]
  , describe "The pseudo functions"
    [ it "should render correctly"
        [ renderCompact ((p .| lang "no") [ borderStyle solid ])
            `shouldEqual` "p:lang(no){border-style:solid}"
        ]
    ]
  ]
