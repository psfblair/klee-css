module Css.BorderTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Border exposing (..)
import Css.Size exposing (..)

import Css.Color exposing (..)
import Css exposing (renderCompact)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.BorderTests"
  [ testBorder ]

testBorder : Spec
testBorder =
  describe "The border functions"
  [ it "should render the simple border properties correctly"
      [ renderCompact (border solid (px 20) aliceblue)
          `shouldEqual` "{border:solid 20px rgba(240,248,255,1)}"
      , renderCompact (borderTop dotted nil aliceblue)
          `shouldEqual` "{border-top:dotted 0 rgba(240,248,255,1)}"
      , renderCompact (borderLeft dashed (unitless 0) aliceblue)
          `shouldEqual` "{border-left:dashed 0 rgba(240,248,255,1)}"
      , renderCompact (borderBottom double (cm 5) aliceblue)
          `shouldEqual` "{border-bottom:double 5cm rgba(240,248,255,1)}"
      , renderCompact (borderRight wavy (mm 2.0) aliceblue)
          `shouldEqual` "{border-right:wavy 2mm rgba(240,248,255,1)}"
      -- Relative widths should not compile; uncomment to see:
      -- , border solid (pct 20) aliceblue `shouldEqual` border solid (pct 20) aliceblue
      -- , borderTop dotted (pct 20) antiquewhite `shouldEqual` borderTop dotted (pct 20) antiquewhite
      -- , borderLeft dashed (pct 0) aqua `shouldEqual` borderLeft dashed (pct 0) aqua
      -- , borderBottom double (pct 5) aquamarine `shouldEqual` borderBottom double (pct 5) aquamarine
      -- , borderRight wavy (pct 2.0) azure `shouldEqual` borderRight wavy (pct 2.0) azure
      ]
    , it "should render the border color properties properly"
      [ renderCompact (borderColor aliceblue)
          `shouldEqual` "{border-color:rgba(240,248,255,1)}"
      , renderCompact (borderLeftColor aliceblue)
          `shouldEqual` "{border-left-color:rgba(240,248,255,1)}"
      , renderCompact (borderRightColor aliceblue)
          `shouldEqual` "{border-right-color:rgba(240,248,255,1)}"
      , renderCompact (borderTopColor aliceblue)
          `shouldEqual` "{border-top-color:rgba(240,248,255,1)}"
      , renderCompact (borderBottomColor aliceblue)
          `shouldEqual` "{border-bottom-color:rgba(240,248,255,1)}"
      , renderCompact (borderColor4 aliceblue aliceblue aliceblue aliceblue)
          `shouldEqual` ("{border-color:rgba(240,248,255,1) rgba(240,248,255,1) " ++
                          "rgba(240,248,255,1) rgba(240,248,255,1)}")
      ]
    , it "should render the border style properties properly"
      [ renderCompact (borderStyle groove)
          `shouldEqual` "{border-style:groove}"
      , renderCompact (borderLeftStyle ridge)
          `shouldEqual` "{border-left-style:ridge}"
      , renderCompact (borderRightStyle inset)
          `shouldEqual` "{border-right-style:inset}"
      , renderCompact (borderTopStyle outset)
          `shouldEqual` "{border-top-style:outset}"
      , renderCompact (borderBottomStyle solid)
          `shouldEqual` "{border-bottom-style:solid}"
      , renderCompact (borderStyle4 solid solid solid solid)
          `shouldEqual` "{border-style:solid solid solid solid}"
      ]
    , it "should render the border width properties properly"
      [ renderCompact (borderWidth (inches 3.8))
          `shouldEqual` "{border-width:3.8in}"
      , renderCompact (borderLeftWidth (px 0.5))
          `shouldEqual` "{border-left-width:0.5px}"
      , renderCompact (borderRightWidth (pt 2))
          `shouldEqual` "{border-right-width:2pt}"
      , renderCompact (borderTopWidth (pc 1))
          `shouldEqual` "{border-top-width:1pc}"
      , renderCompact (borderBottomWidth (pc 3.4))
          `shouldEqual` "{border-bottom-width:3.4pc}"
      , renderCompact (borderWidth4 (px 5.2) (px 7.6) (px 5) (px 5.4))
          `shouldEqual` "{border-width:5.2px 7.6px 5px 5.4px}"
      -- Relative widths should not compile; uncomment to see:
      -- , borderWidth (em 3.8) `shouldEqual` borderWidth (em 3.8)
      -- , borderLeftWidth (pct 0.5) `shouldEqual` borderLeftWidth (pct 0.5)
      -- , borderRightWidth (pct 2) `shouldEqual` borderRightWidth (pct 2)
      -- , borderTopWidth (ex 1) `shouldEqual` borderTopWidth (ex 1)
      -- , borderBottomWidth (em 3.4) `shouldEqual` borderBottomWidth (em 3.4)
      -- , borderWidth4 (ex 5.2) (pct 7.6) (srem 5) (vw 5.4)
      --     `shouldEqual` borderWidth4 (ex 5.2) (pct 7.6) (srem 5) (vw 5.4)
      ]
  ]

testOutline : Spec
testOutline =
  describe "The outline functions"
  [ it "should render the simple outline properties correctly"
    [ renderCompact(outline solid (px 20) aliceblue)
        `shouldEqual` "{outline:solid 20px rgba(240,248,255,1)}"
    , renderCompact(outlineTop solid (px 20) aliceblue)
        `shouldEqual` "{outline:solid 20px rgba(240,248,255,1)}"
    , renderCompact(outlineLeft solid (px 20) aliceblue)
        `shouldEqual` "{outline:solid 20px rgba(240,248,255,1)}"
    , renderCompact(outlineBottom solid (px 20) aliceblue)
        `shouldEqual` "{outline:solid 20px rgba(240,248,255,1)}"
    , renderCompact(outlineRight solid (px 20) aliceblue)
        `shouldEqual` "{outline:solid 20px rgba(240,248,255,1)}"
    -- Relative widths should not compile; uncomment to see:
    -- , outline solid (pct 20) aliceblue `shouldEqual` outline solid (pct 20) aliceblue
    -- , outlineTop solid (pct 20) aliceblue `shouldEqual` outlineTop solid (pct 20) aliceblue
    -- , outlineLeft solid (pct 20) aliceblue `shouldEqual` outlineLeft solid (pct 20) aliceblue
    -- , outlineBottom solid (pct 20) aliceblue `shouldEqual` outlineBottom solid (pct 20) aliceblue
    -- , outlineRight solid (pct 20) aliceblue `shouldEqual` outlineRight solid (pct 20) aliceblue
    ]
  ]



    {-
-}

{-
testBorder : Spec
testBorder =
  describe "The outline functions"
  [ it "should render the simple outline properties correctly"
    [ `shouldEqual`
    ,
    ]
  ]

-}
{-

  -- * Outline properties.

  ,
  , outlineColor4, outlineColor, outlineLeftColor, outlineRightColor, outlineTopColor, outlineBottomColor
  , outlineStyle4, outlineStyle, outlineLeftStyle, outlineRightStyle, outlineTopStyle, outlineBottomStyle
  , outlineWidth4, outlineWidth, outlineLeftWidth, outlineRightWidth, outlineTopWidth, outlineBottomWidth
  , outlineOffset

  -- * Border radius.

  , borderRadius
  , borderTopLeftRadius, borderTopRightRadius
  , borderBottomLeftRadius, borderBottomRightRadius

  -- * Collapsing borders model for a table
  , borderCollapse
  , borderSpacing, borderSpacing2

-}
