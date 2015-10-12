module Css.BorderTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Border exposing (..)
import Css.Size exposing (..)
import Css.Color exposing (..)
import Css.Display exposing (collapse)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.BorderTests"
  [ describe "The border functions"
    [ it "should render the simple border properties correctly"
        [ renderProperties [border solid (px 20) aliceblue]
            `shouldEqual` "border:solid 20px rgba(240,248,255,1)"
        , renderProperties [borderTop dotted nil aliceblue]
            `shouldEqual` "border-top:dotted 0 rgba(240,248,255,1)"
        , renderProperties [borderLeft dashed (unitless 0) aliceblue]
            `shouldEqual` "border-left:dashed 0 rgba(240,248,255,1)"
        , renderProperties [borderBottom double (cm 5) aliceblue]
            `shouldEqual` "border-bottom:double 5cm rgba(240,248,255,1)"
        , renderProperties [borderRight wavy (mm 2.0) aliceblue]
            `shouldEqual` "border-right:wavy 2mm rgba(240,248,255,1)"
        -- Relative widths should not compile; uncomment to see:
        -- , border solid (pct 20) aliceblue `shouldEqual` border solid (pct 20) aliceblue
        -- , borderTop dotted (pct 20) antiquewhite `shouldEqual` borderTop dotted (pct 20) antiquewhite
        -- , borderLeft dashed (pct 0) aqua `shouldEqual` borderLeft dashed (pct 0) aqua
        -- , borderBottom double (pct 5) aquamarine `shouldEqual` borderBottom double (pct 5) aquamarine
        -- , borderRight wavy (pct 2.0) azure `shouldEqual` borderRight wavy (pct 2.0) azure
        ]
      , it "should render the border color properties properly"
        [ renderProperties [borderColor aliceblue]
            `shouldEqual` "border-color:rgba(240,248,255,1)"
        , renderProperties [borderLeftColor aliceblue]
            `shouldEqual` "border-left-color:rgba(240,248,255,1)"
        , renderProperties [borderRightColor aliceblue]
            `shouldEqual` "border-right-color:rgba(240,248,255,1)"
        , renderProperties [borderTopColor aliceblue]
            `shouldEqual` "border-top-color:rgba(240,248,255,1)"
        , renderProperties [borderBottomColor aliceblue]
            `shouldEqual` "border-bottom-color:rgba(240,248,255,1)"
        , renderProperties [borderColor4 aliceblue black blue white]
            `shouldEqual` ("border-color:rgba(240,248,255,1) rgba(0,0,0,1) " ++
                            "rgba(0,0,255,1) rgba(255,255,255,1)")
        ]
      , it "should render the border style properties properly"
        [ renderProperties [borderStyle groove]
            `shouldEqual` "border-style:groove"
        , renderProperties [borderLeftStyle ridge]
            `shouldEqual` "border-left-style:ridge"
        , renderProperties [borderRightStyle inset]
            `shouldEqual` "border-right-style:inset"
        , renderProperties [borderTopStyle outset]
            `shouldEqual` "border-top-style:outset"
        , renderProperties [borderBottomStyle solid]
            `shouldEqual` "border-bottom-style:solid"
        , renderProperties [borderStyle4 solid solid solid solid]
            `shouldEqual` "border-style:solid solid solid solid"
        ]
      , it "should render the border width properties properly"
        [ renderProperties [borderWidth (inches 3.8)]
            `shouldEqual` "border-width:3.8in"
        , renderProperties [borderLeftWidth (px 0.5)]
            `shouldEqual` "border-left-width:0.5px"
        , renderProperties [borderRightWidth (pt 2)]
            `shouldEqual` "border-right-width:2pt"
        , renderProperties [borderTopWidth (pc 1)]
            `shouldEqual` "border-top-width:1pc"
        , renderProperties [borderBottomWidth (pc 3.4)]
            `shouldEqual` "border-bottom-width:3.4pc"
        , renderProperties [borderWidth4 (px 5.2) (px 7.6) (px 5) (px 5.4)]
            `shouldEqual` "border-width:5.2px 7.6px 5px 5.4px"
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
  , describe "The outline functions"
    [ it "should render the simple outline properties correctly"
      [ renderProperties [outline solid (px 20) aliceblue ]
          `shouldEqual` "outline:solid 20px rgba(240,248,255,1)"
      , renderProperties [outlineTop solid (px 20) aliceblue]
          `shouldEqual` "outline-top:solid 20px rgba(240,248,255,1)"
      , renderProperties [outlineLeft solid (px 20) aliceblue]
          `shouldEqual` "outline-left:solid 20px rgba(240,248,255,1)"
      , renderProperties [outlineBottom solid (px 20) aliceblue]
          `shouldEqual` "outline-bottom:solid 20px rgba(240,248,255,1)"
      , renderProperties [outlineRight solid (px 20) aliceblue]
          `shouldEqual` "outline-right:solid 20px rgba(240,248,255,1)"
      -- Relative widths should not compile; uncomment to see:
      -- , outline solid (pct 20) aliceblue `shouldEqual` outline solid (pct 20) aliceblue
      -- , outlineTop solid (pct 20) aliceblue `shouldEqual` outlineTop solid (pct 20) aliceblue
      -- , outlineLeft solid (pct 20) aliceblue `shouldEqual` outlineLeft solid (pct 20) aliceblue
      -- , outlineBottom solid (pct 20) aliceblue `shouldEqual` outlineBottom solid (pct 20) aliceblue
      -- , outlineRight solid (pct 20) aliceblue `shouldEqual` outlineRight solid (pct 20) aliceblue
      ]
    , it "should render the outline color properties correctly"
      [ renderProperties [outlineColor aliceblue]
          `shouldEqual` "outline-color:rgba(240,248,255,1)"
      , renderProperties [outlineLeftColor aliceblue]
          `shouldEqual` "outline-left-color:rgba(240,248,255,1)"
      , renderProperties [outlineRightColor aliceblue]
          `shouldEqual` "outline-right-color:rgba(240,248,255,1)"
      , renderProperties [outlineTopColor aliceblue]
          `shouldEqual` "outline-top-color:rgba(240,248,255,1)"
      , renderProperties [outlineBottomColor aliceblue]
          `shouldEqual` "outline-bottom-color:rgba(240,248,255,1)"
      , renderProperties [outlineColor4 aliceblue black blue white]
          `shouldEqual` ("outline-color:rgba(240,248,255,1) rgba(0,0,0,1) " ++
                          "rgba(0,0,255,1) rgba(255,255,255,1)")
      ]
    , it "should render the outline style properties properly"
      [ renderProperties [outlineStyle solid]
          `shouldEqual` "outline-style:solid"
      , renderProperties [outlineLeftStyle solid]
          `shouldEqual` "outline-left-style:solid"
      , renderProperties [outlineRightStyle solid]
          `shouldEqual` "outline-right-style:solid"
      , renderProperties [outlineTopStyle solid]
          `shouldEqual` "outline-top-style:solid"
      , renderProperties [outlineBottomStyle solid]
          `shouldEqual` "outline-bottom-style:solid"
      , renderProperties [outlineStyle4 solid dashed dotted groove]
          `shouldEqual` "outline-style:solid dashed dotted groove"
      ]
    , it "should render the outline width properties properly"
      [ renderProperties [outlineWidth (px 20)]
          `shouldEqual` "outline-width:20px"
      , renderProperties [outlineLeftWidth (px 20)]
          `shouldEqual` "outline-left-width:20px"
      , renderProperties [outlineRightWidth (px 20)]
          `shouldEqual` "outline-right-width:20px"
      , renderProperties [outlineTopWidth (px 20)]
          `shouldEqual` "outline-top-width:20px"
      , renderProperties [outlineBottomWidth (px 20)]
          `shouldEqual` "outline-bottom-width:20px"
      , renderProperties [outlineWidth4 (px 20) (px 30) (px 40) (px 50)]
          `shouldEqual` "outline-width:20px 30px 40px 50px"
      -- Relative widths should not compile; uncomment to see:
      -- , outlineWidth (pct 20) `shouldEqual` outlineWidth (pct 20)
      -- , outlineLeftWidth (pct 20) `shouldEqual` outlineLeftWidth (pct 20)
      -- , outlineRightWidth (pct 20) `shouldEqual` outlineRightWidth (pct 20)
      -- , outlineTopWidth (pct 20) `shouldEqual` outlineTopWidth (pct 20)
      -- , outlineBottomWidth (pct 20) `shouldEqual` outlineBottomWidth (pct 20)
      -- , outlineWidth4 (pct 20) (pct 20) (pct 20) (pct 20)
      --         `shouldEqual` outlineWidth4 (pct 20) (pct 20) (pct 20) (pct 20)
      ]
    , it "should render the outline offset property properly"
      [ renderProperties [outlineOffset (px 20)]
          `shouldEqual` "outline-offset:20px"
      ]
    ]
  , describe "The border radius functions"
    [ it "should render the border radius properties correctly"
      [ renderProperties [borderRadius (px 20) (pct 30) (px 40) (px 50)]
          `shouldEqual` "border-radius:20px 30% 40px 50px"
      , renderProperties [borderTopLeftRadius (px 20) (pct 30)]
          `shouldEqual` "border-top-left-radius:20px 30%"
      , renderProperties [borderTopRightRadius (px 20) (pct 30)]
          `shouldEqual` "border-top-right-radius:20px 30%"
      , renderProperties [borderBottomLeftRadius (px 20) (pct 30)]
          `shouldEqual` "border-bottom-left-radius:20px 30%"
      , renderProperties [borderBottomRightRadius (px 20) (pct 30)]
          `shouldEqual` "border-bottom-right-radius:20px 30%"
      ]
    ]
  , describe "The border collapse function"
    [ it "should render the border collapse property correctly"
      [ renderProperties [borderCollapse collapse]
          `shouldEqual` "border-collapse:collapse"
      ]
    ]
  , describe "The border spacing functions"
    [ it "should render the border spacing properties correctly"
      [ renderProperties [borderSpacing (px 20)]
           `shouldEqual` "border-spacing:20px"
      ,  renderProperties [borderSpacing (pct 20)]
           `shouldEqual` "border-spacing:20%"
      , renderProperties [borderSpacing2 (px 20) (pct 30)]
          `shouldEqual` "border-spacing:20px 30%"
      ]
    ]
  ]
