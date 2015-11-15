module Css.BorderTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Border exposing (..)
import Css.Color exposing (..)
import Css.Display exposing (collapse)
import Css.Geometry exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------
-- TODO Test generic properties + invert for border color

suite : Spec
suite = describe "Css.BorderTests"
  [ describe "The border functions"
    [ it "should render the simple border properties correctly"
      [ renderProperties [border solid (px 20) green]
          `shouldEqual` "border:solid 20px #73D216"
      , renderProperties [borderTop dotted abs0 green]
          `shouldEqual` "border-top:dotted 0 #73D216"
      , renderProperties [borderBottom double (cm 5) green]
          `shouldEqual` "border-bottom:double 5cm #73D216"
      , renderProperties [borderRight wavy (mm 2.0) green]
          `shouldEqual` "border-right:wavy 2mm #73D216"
      , renderProperties [borderRight wavy (mm 2.0) currentColor]
          `shouldEqual` "border-right:wavy 2mm currentColor"
      -- Relative widths should not compile; uncomment to see:
      -- , border solid (pct 20) green `shouldEqual` border solid (pct 20) green
      -- , borderTop dotted (pct 20) antiquewhite `shouldEqual` borderTop dotted (pct 20) antiquewhite
      -- , borderLeft dashed (pct 0) aqua `shouldEqual` borderLeft dashed (pct 0) aqua
      -- , borderBottom double (pct 5) aquamarine `shouldEqual` borderBottom double (pct 5) aquamarine
      -- , borderRight wavy (pct 2.0) azure `shouldEqual` borderRight wavy (pct 2.0) azure
      ]
    , it "should render the border color properties properly"
      [ renderProperties [borderColor green]
          `shouldEqual` "border-color:#73D216"
      , renderProperties [borderLeftColor green]
          `shouldEqual` "border-left-color:#73D216"
      , renderProperties [borderRightColor green]
          `shouldEqual` "border-right-color:#73D216"
      , renderProperties [borderTopColor green]
          `shouldEqual` "border-top-color:#73D216"
      , renderProperties [borderBottomColor green]
          `shouldEqual` "border-bottom-color:#73D216"
      , renderProperties [borderColor4 green black blue white]
          `shouldEqual` ("border-color:#73D216 #000000 " ++
                          "#3465A4 #FFFFFF")
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
-- TODO Test thin, thick, medium, generics        
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
      [ renderProperties [outline solid (px 20) green]
          `shouldEqual` "outline:solid 20px #73D216"
      , renderProperties [outline solid (px 20) invert]
          `shouldEqual` "outline:solid 20px invert"
      -- Relative widths should not compile; uncomment to see:
      -- , outline solid (pct 20) green `shouldEqual` outline solid (pct 20) green
      ]      
    , it "should render the outline color properties correctly"
      [ renderProperties [outlineColor green]
          `shouldEqual` "outline-color:#73D216"
      , renderProperties [outlineColor invert]
          `shouldEqual` "outline-color:invert"
      ]
    , it "should render the outline style properties properly"
      [ renderProperties [outlineStyle solid]
          `shouldEqual` "outline-style:solid"
      ]      
    , it "should render the outline width properties properly"
      [ renderProperties [outlineWidth (px 20)]
          `shouldEqual` "outline-width:20px"
      , renderProperties [outlineWidth thin]
            `shouldEqual` "outline-width:thin"
      , renderProperties [outlineWidth medium]
            `shouldEqual` "outline-width:medium"
      , renderProperties [outlineWidth thick]
            `shouldEqual` "outline-width:thick"
      -- Relative widths should not compile; uncomment to see:
      -- , outlineWidth (pct 20) `shouldEqual` outlineWidth (pct 20)
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
