module Css.BorderTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Border exposing (..)
import Css.Color exposing (..)
import Css.Common exposing (..)
import Css.Display exposing (collapse)
import Css.Geometry exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.BorderTests"
  [ describe "The border function"
    [ it "should render a composite border property correctly"
      [ renderProperties [ border (aBorderWith solid (px 20) green) ]
          `shouldEqual` "border:solid 20px #73D216"
      , renderProperties [ border (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border:dotted 0 transparent"
      , renderProperties [ border (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border:dotted 0 transparent"
      -- , renderProperties [ border solid (pct 20) green ] 
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border (aBorderWith inherit (px 20) green) ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border (aBorderWith solid inherit green) ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border (aBorderWith solid (px 20) inherit) ]
      --     `shouldEqual` "border:should not compile"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ border initial ] `shouldEqual` "border:initial"
      , renderProperties [ border inherit ] `shouldEqual` "border:inherit"
      , renderProperties [ border unset ] `shouldEqual` "border:unset"
      , renderProperties [ border (other "foo") ] 
          `shouldEqual` "border:foo"
      , renderProperties [ border (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "border:-webkit-foo;border:-moz-foo"
      -- , renderProperties [ border all ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border auto ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border baseline ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border center ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border normal ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border none ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border visible ]
      --     `shouldEqual` "border:should not compile"
      -- , renderProperties [ border hidden ]
      --     `shouldEqual` "border:should not compile"
      ]
    ]
  , describe "The borderTop function"
    [ it "should render a composite border property correctly"
      [ renderProperties [ borderTop (aBorderWith solid (px 20) green) ]
          `shouldEqual` "border-top:solid 20px #73D216"
      , renderProperties [ borderTop (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-top:dotted 0 transparent"
      , renderProperties [ borderTop (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-top:dotted 0 transparent"
      -- , renderProperties [ borderTop solid (pct 20) green ] 
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop (aBorderWith inherit (px 20) green) ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop (aBorderWith solid inherit green) ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop (aBorderWith solid (px 20) inherit) ]
      --     `shouldEqual` "border-top:should not compile"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ borderTop initial ] `shouldEqual` "border-top:initial"
      , renderProperties [ borderTop inherit ] `shouldEqual` "border-top:inherit"
      , renderProperties [ borderTop unset ] `shouldEqual` "border-top:unset"
      , renderProperties [ borderTop (other "foo") ] 
          `shouldEqual` "border-top:foo"
      , renderProperties [ borderTop (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "border-top:-webkit-foo;border-top:-moz-foo"
      -- , renderProperties [ borderTop all ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop auto ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop baseline ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop center ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop normal ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop none ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop visible ]
      --     `shouldEqual` "border-top:should not compile"
      -- , renderProperties [ borderTop hidden ]
      --     `shouldEqual` "border-top:should not compile"
      ]
    ]
  , describe "The borderBottom function"
    [ it "should render a composite border property correctly"
      [ renderProperties [ borderBottom (aBorderWith solid (px 20) green) ]
          `shouldEqual` "border-bottom:solid 20px #73D216"
      , renderProperties [ borderBottom (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-bottom:dotted 0 transparent"
      , renderProperties [ borderBottom (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-bottom:dotted 0 transparent"
      -- , renderProperties [ borderBottom solid (pct 20) green ] 
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom (aBorderWith inherit (px 20) green) ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom (aBorderWith solid inherit green) ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom (aBorderWith solid (px 20) inherit) ]
      --     `shouldEqual` "border-bottom:should not compile"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ borderBottom initial ] 
          `shouldEqual` "border-bottom:initial"
      , renderProperties [ borderBottom inherit ] 
          `shouldEqual` "border-bottom:inherit"
      , renderProperties [ borderBottom unset ] 
          `shouldEqual` "border-bottom:unset"
      , renderProperties [ borderBottom (other "foo") ] 
          `shouldEqual` "border-bottom:foo"
      , renderProperties [ borderBottom (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "border-bottom:-webkit-foo;border-bottom:-moz-foo"
      -- , renderProperties [ borderBottom all ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom auto ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom baseline ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom center ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom normal ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom none ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom visible ]
      --     `shouldEqual` "border-bottom:should not compile"
      -- , renderProperties [ borderBottom hidden ]
      --     `shouldEqual` "border-bottom:should not compile"
      ]
    ]
  , describe "The borderLeft function"
    [ it "should render a composite border property correctly"
      [ renderProperties [ borderLeft (aBorderWith solid (px 20) green) ]
          `shouldEqual` "border-left:solid 20px #73D216"
      , renderProperties [ borderLeft (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-left:dotted 0 transparent"
      , renderProperties [ borderLeft (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-left:dotted 0 transparent"
      -- , renderProperties [ borderLeft solid (pct 20) green ] 
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft (aBorderWith inherit (px 20) green) ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft (aBorderWith solid inherit green) ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft (aBorderWith solid (px 20) inherit) ]
      --     `shouldEqual` "border-left:should not compile"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ borderLeft initial ] 
          `shouldEqual` "border-left:initial"
      , renderProperties [ borderLeft inherit ] 
          `shouldEqual` "border-left:inherit"
      , renderProperties [ borderLeft unset ] 
          `shouldEqual` "border-left:unset"
      , renderProperties [ borderLeft (other "foo") ] 
          `shouldEqual` "border-left:foo"
      , renderProperties [ borderLeft (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "border-left:-webkit-foo;border-left:-moz-foo"
      -- , renderProperties [ borderLeft all ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft auto ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft baseline ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft center ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft normal ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft none ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft visible ]
      --     `shouldEqual` "border-left:should not compile"
      -- , renderProperties [ borderLeft hidden ]
      --     `shouldEqual` "border-left:should not compile"
      ]
    ]
  , describe "The borderRight function"
    [ it "should render a composite border property correctly"
      [ renderProperties [ borderRight (aBorderWith solid (px 20) green) ]
          `shouldEqual` "border-right:solid 20px #73D216"
      , renderProperties [ borderRight (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-right:dotted 0 transparent"
      , renderProperties [ borderRight (aBorderWith dotted abs0 transparent) ]
          `shouldEqual` "border-right:dotted 0 transparent"
      -- , renderProperties [ borderRight solid (pct 20) green ] 
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight (aBorderWith inherit (px 20) green) ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight (aBorderWith solid inherit green) ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight (aBorderWith solid (px 20) inherit) ]
      --     `shouldEqual` "border-right:should not compile"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ borderRight initial ] 
          `shouldEqual` "border-right:initial"
      , renderProperties [ borderRight inherit ] 
          `shouldEqual` "border-right:inherit"
      , renderProperties [ borderRight unset ] 
          `shouldEqual` "border-right:unset"
      , renderProperties [ borderRight (other "foo") ] 
          `shouldEqual` "border-right:foo"
      , renderProperties [ borderRight (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "border-right:-webkit-foo;border-right:-moz-foo"
      -- , renderProperties [ borderRight all ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight auto ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight baseline ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight center ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight normal ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight none ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight visible ]
      --     `shouldEqual` "border-right:should not compile"
      -- , renderProperties [ borderRight hidden ]
      --     `shouldEqual` "border-right:should not compile"
      ]
    ]
  , describe "the borderColor function" 
    [ it "should render the border color properties properly"
      [ renderProperties [borderColor green]
          `shouldEqual` "border-color:#73D216"
      , renderProperties [borderColor transparent]
          `shouldEqual` "border-color:transparent"
      , renderProperties [borderLeftColor green]
          `shouldEqual` "border-left-color:#73D216"
      , renderProperties [borderLeftColor transparent]
          `shouldEqual` "border-left-color:transparent"
      , renderProperties [borderRightColor green]
          `shouldEqual` "border-right-color:#73D216"
      , renderProperties [borderRightColor transparent]
          `shouldEqual` "border-right-color:transparent"
      , renderProperties [borderTopColor green]
          `shouldEqual` "border-top-color:#73D216"
      , renderProperties [borderTopColor transparent]
          `shouldEqual` "border-top-color:transparent"
      , renderProperties [borderBottomColor green]
          `shouldEqual` "border-bottom-color:#73D216"
      , renderProperties [borderBottomColor transparent]
          `shouldEqual` "border-bottom-color:transparent"
      , renderProperties [borderColor4 green black blue white]
          `shouldEqual` ("border-color:#73D216 #000000 " ++
                          "#3465A4 #FFFFFF")
      ]
    ]
  , describe "the borderStyle function" 
    [ it "should render the border style properties properly"
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
    ]
  , describe "the borderWidth function" 
    [ it "should render the border width properties properly"
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
      [ renderProperties [ outline (anOutlineWith solid (px 20) green) ]
          `shouldEqual` "outline:solid 20px #73D216"
      , renderProperties [ outline (anOutlineWith solid (px 20) invert) ]
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
    --TODO add new functions and generics
  , describe "The border radius functions"
    [ it "should render the border radius properties correctly"
      [ renderProperties [borderRadius4 (px 20) (pct 30) (px 40) (px 50)]
          `shouldEqual` "border-radius:20px 30% 40px 50px"
      , renderProperties [borderTopLeftRadius2 (px 20) (pct 30)]
          `shouldEqual` "border-top-left-radius:20px 30%"
      , renderProperties [borderTopRightRadius2 (px 20) (pct 30)]
          `shouldEqual` "border-top-right-radius:20px 30%"
      , renderProperties [borderBottomLeftRadius2 (px 20) (pct 30)]
          `shouldEqual` "border-bottom-left-radius:20px 30%"
      , renderProperties [borderBottomRightRadius2 (px 20) (pct 30)]
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
