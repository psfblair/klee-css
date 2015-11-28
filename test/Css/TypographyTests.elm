module Css.TypographyTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.ColorsAndStrokes exposing (..)
import Css.Common exposing (..)
import Css.Geometry exposing (unitless, px, pct, em, sideLeft)
import Css exposing (renderProperties)

import Css.Typography exposing (..)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.TextTests"
  [ describe "the font color properties"
    [ it "can accept a named color"
      [ renderProperties [fontColor green] `shouldEqual` "color:#73D216"
      , renderProperties [color green] `shouldEqual` "color:#73D216"
      , renderProperties [fontColor currentColor] `shouldEqual` "color:currentColor"
      , renderProperties [color currentColor] `shouldEqual` "color:currentColor"
      ]
    , it "can accept common properties initial, inherit, unset and other"
      [ renderProperties [fontColor initial] `shouldEqual` "color:initial"
      , renderProperties [color initial] `shouldEqual` "color:initial"
      , renderProperties [fontColor inherit] `shouldEqual` "color:inherit"
      , renderProperties [color inherit] `shouldEqual` "color:inherit"
      , renderProperties [fontColor unset] `shouldEqual` "color:unset" 
      , renderProperties [color unset] `shouldEqual` "color:unset" 
      , renderProperties [fontColor (other "wild-honey")] 
          `shouldEqual` "color:wild-honey" 
      , renderProperties [color (other "wild-honey")] 
          `shouldEqual` "color:wild-honey" 
      , renderProperties [fontColor (otherPrefixed [webkit_, moz_] "wild-honey")] 
          `shouldEqual` "color:-webkit-wild-honey;color:-moz-wild-honey" 
      , renderProperties [color (otherPrefixed [webkit_, moz_] "wild-honey")] 
          `shouldEqual` "color:-webkit-wild-honey;color:-moz-wild-honey" 
      -- Should not compile:
      -- , renderProperties [fontColor all]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [color all]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor auto]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [color auto]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor baseline]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [color baseline]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor center]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [color center]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor normal]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [color normal]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor none]
      --     `shouldEqual` "color:should not compile"      
      -- , renderProperties [color none]
      --     `shouldEqual` "color:should not compile"      
      -- , renderProperties [fontColor visible]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [color visible]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor hidden]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [color hidden]
      --     `shouldEqual` "color:should not compile"
      ]
    ]
  , describe "fontFamily"
    [ it "should render font families properly"
      [ renderProperties [ fontFamily (family "Bingo") ] 
          `shouldEqual` "font-family:\"Bingo\""
      , renderProperties [ fontFamily (families ["Bingo"] [sansSerif]) ] 
          `shouldEqual` "font-family:\"Bingo\",sans-serif"
      , renderProperties 
          [ fontFamily (families ["Bingo", "Weyerhauser"] [serif, fantasy]) ] 
            `shouldEqual` "font-family:\"Bingo\",\"Weyerhauser\",serif,fantasy"
      ]
    , it "can accept common properties initial, inherit, unset and other"
      [ renderProperties [fontFamily initial] `shouldEqual` "font-family:initial"
      , renderProperties [fontFamily inherit] `shouldEqual` "font-family:inherit" 
      , renderProperties [fontFamily unset]   `shouldEqual` "font-family:unset"          
      , renderProperties [fontFamily (other "wild-honey")] 
          `shouldEqual` "font-family:wild-honey" 
      , renderProperties [fontFamily (otherPrefixed [webkit_, moz_] "wild-honey")] 
          `shouldEqual` "font-family:-webkit-wild-honey;font-family:-moz-wild-honey" 
      -- Should not compile:
      -- , renderProperties [fontFamily all]
      --     `shouldEqual` "font-family:should not compile"
      -- , renderProperties [fontFamily auto]
      --     `shouldEqual` "font-family:should not compile"
      -- , renderProperties [fontFamily baseline]
      --     `shouldEqual` "font-family:should not compile"
      -- , renderProperties [fontFamily center]
      --     `shouldEqual` "font-family:should not compile"
      -- , renderProperties [fontFamily normal]
      --     `shouldEqual` "font-family:should not compile"
      -- , renderProperties [fontFamily none]
      --     `shouldEqual` "font-family:should not compile"      
      -- , renderProperties [fontFamily visible]
      --     `shouldEqual` "font-family:should not compile"
      -- , renderProperties [fontFamily hidden]
      --     `shouldEqual` "font-family:should not compile"
      ]
    ]
  , describe "fontSize"
    [ it "should render named font sizes properly"
      [ renderProperties [ fontSize xxSmall ] 
          `shouldEqual` "font-size:xx-small"
      ]
    , it "should render numeric font sizes properly"
      [ renderProperties [ fontSize (px 20) ] 
          `shouldEqual` "font-size:20px"
      , renderProperties [ fontSize (em 0.8) ] 
          `shouldEqual` "font-size:0.8em" 
      ]
    , it "can accept common properties initial, inherit, unset and other"
      [ renderProperties [fontSize initial] `shouldEqual` "font-size:initial"
      , renderProperties [fontSize inherit] `shouldEqual` "font-size:inherit" 
      , renderProperties [fontSize unset] `shouldEqual` "font-size:unset" 
      , renderProperties [fontSize (other "foo")] `shouldEqual` "font-size:foo" 
      , renderProperties [fontSize (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "font-size:-webkit-foo;font-size:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [fontSize all]
      --     `shouldEqual` "font-size:should not compile"
      -- , renderProperties [fontSize auto]
      --     `shouldEqual` "font-size:should not compile"
      -- , renderProperties [fontSize baseline]
      --     `shouldEqual` "font-size:should not compile"
      -- , renderProperties [fontSize center]
      --     `shouldEqual` "font-size:should not compile"
      -- , renderProperties [fontSize normal]
      --     `shouldEqual` "font-size:should not compile"
      -- , renderProperties [fontSize none]
      --     `shouldEqual` "font-size:should not compile"      
      -- , renderProperties [fontSize visible]
      --     `shouldEqual` "font-size:should not compile"
      -- , renderProperties [fontSize hidden]
      --     `shouldEqual` "font-size:should not compile"
      ]
    ]
  , describe "fontStyle"
    [ it "should render named font styles properly"
      [ renderProperties [ fontStyle italic ] 
          `shouldEqual` "font-style:italic"
      ]
    , it "can accept common properties normal, initial, inherit, unset and other"
      [ renderProperties [fontStyle initial] `shouldEqual` "font-style:initial"
      , renderProperties [fontStyle inherit] `shouldEqual` "font-style:inherit" 
      , renderProperties [fontStyle normal] `shouldEqual` "font-style:normal" 
      , renderProperties [fontStyle unset] `shouldEqual` "font-style:unset" 
      , renderProperties [fontStyle (other "foo")] `shouldEqual` "font-style:foo" 
      , renderProperties [fontStyle (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "font-style:-webkit-foo;font-style:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [fontStyle all]
      --     `shouldEqual` "font-style:should not compile"
      -- , renderProperties [fontStyle auto]
      --     `shouldEqual` "font-style:should not compile"
      -- , renderProperties [fontStyle baseline]
      --     `shouldEqual` "font-style:should not compile"
      -- , renderProperties [fontStyle center]
      --     `shouldEqual` "font-style:should not compile"
      -- , renderProperties [fontStyle none]
      --     `shouldEqual` "font-style:should not compile"      
      -- , renderProperties [fontStyle visible]
      --     `shouldEqual` "font-style:should not compile"
      -- , renderProperties [fontStyle hidden]
      --     `shouldEqual` "font-style:should not compile"
      ]
    ]
  , describe "fontVariant"
    [ it "should render named font variants properly"
      [ renderProperties [ fontVariant smallCaps ] 
          `shouldEqual` "font-variant:small-caps"
      ]    
    , it "can accept common properties normal, initial, inherit, unset and other"
      [ renderProperties [fontVariant initial] `shouldEqual` "font-variant:initial"
      , renderProperties [fontVariant inherit] `shouldEqual` "font-variant:inherit" 
      , renderProperties [fontVariant normal] `shouldEqual` "font-variant:normal" 
      , renderProperties [fontVariant none] `shouldEqual` "font-variant:none"      
      , renderProperties [fontVariant unset] `shouldEqual` "font-variant:unset" 
      , renderProperties [fontVariant (other "foo")] `shouldEqual` "font-variant:foo" 
      , renderProperties [fontVariant (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "font-variant:-webkit-foo;font-variant:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [fontVariant all]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant auto]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant baseline]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant center]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant visible]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant hidden]
      --     `shouldEqual` "font-variant:should not compile"
      ]
    ]
  , describe "fontWeight"
    [ it "should render named font weights properly"
      [ renderProperties [ fontWeight bold ] 
          `shouldEqual` "font-weight:bold"
      , renderProperties [ fontWeight lighter ] 
          `shouldEqual` "font-weight:lighter"
      ]    
    , it "should render numeric font weights properly"
      [ renderProperties [ fontWeight (weight 100) ] 
          `shouldEqual` "font-weight:100"
      ]    
    , it "can accept common properties normal, initial, inherit, unset and other"
      [ renderProperties [fontWeight initial] `shouldEqual` "font-weight:initial"
      , renderProperties [fontWeight inherit] `shouldEqual` "font-weight:inherit" 
      , renderProperties [fontWeight normal] `shouldEqual` "font-weight:normal" 
      , renderProperties [fontWeight unset] `shouldEqual` "font-weight:unset" 
      , renderProperties [fontWeight (other "foo")] `shouldEqual` "font-weight:foo" 
      , renderProperties [fontWeight (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "font-weight:-webkit-foo;font-weight:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [fontWeight all]
      --     `shouldEqual` "font-weight:should not compile"
      -- , renderProperties [fontWeight auto]
      --     `shouldEqual` "font-weight:should not compile"
      -- , renderProperties [fontWeight baseline]
      --     `shouldEqual` "font-weight:should not compile"
      -- , renderProperties [fontWeight center]
      --     `shouldEqual` "font-weight:should not compile"
      -- , renderProperties [fontWeight none]
      --     `shouldEqual` "font-weight:should not compile"      
      -- , renderProperties [fontWeight visible]
      --     `shouldEqual` "font-weight:should not compile"
      -- , renderProperties [fontWeight hidden]
      --     `shouldEqual` "font-weight:should not compile"
      ]
    ]
  , describe "lineHeight"
    [ it "should render quantitative line heights properly"
      [ renderProperties [ lineHeight (unitless 3.5) ] 
          `shouldEqual` "line-height:3.5"
      , renderProperties [lineHeight (px 20)] 
        `shouldEqual` "line-height:20px" 
      , renderProperties [lineHeight (pct 20)] 
        `shouldEqual` "line-height:20%" 
      ]
    , it "can accept common properties normal, initial, inherit, unset and other"
      [ renderProperties [lineHeight initial] `shouldEqual` "line-height:initial"
      , renderProperties [lineHeight inherit] `shouldEqual` "line-height:inherit" 
      , renderProperties [lineHeight normal] `shouldEqual` "line-height:normal" 
      , renderProperties [lineHeight unset] `shouldEqual` "line-height:unset" 
      , renderProperties [lineHeight (other "foo")] `shouldEqual` "line-height:foo" 
      , renderProperties [lineHeight (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "line-height:-webkit-foo;line-height:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [lineHeight all]
      --     `shouldEqual` "line-height:should not compile"
      -- , renderProperties [lineHeight auto]
      --     `shouldEqual` "line-height:should not compile"
      -- , renderProperties [lineHeight baseline]
      --     `shouldEqual` "line-height:should not compile"
      -- , renderProperties [lineHeight center]
      --     `shouldEqual` "line-height:should not compile"
      -- , renderProperties [lineHeight none]
      --     `shouldEqual` "line-height:should not compile"      
      -- , renderProperties [lineHeight visible]
      --     `shouldEqual` "line-height:should not compile"
      -- , renderProperties [lineHeight hidden]
      --     `shouldEqual` "line-height:should not compile"
      ]
    ]
  , describe "font"
    [ it "will accept a named font"
      [ renderProperties [font caption] `shouldEqual` "font:caption"
      ]
    , it "will accept a composed font"
      [ renderProperties [font (aFont (px 20) ["Lubalin Graph Medium"] [fantasy])] 
          `shouldEqual` "font:20px \"Lubalin Graph Medium\",fantasy"
      , renderProperties 
          [font (aFont (px 20) ["Hobo"] [] 
                |> withWeight bold 
                |> withLineHeight (unitless 3)
                |> withVariant smallCaps
                |> withStyle italic)] 
            `shouldEqual` "font:italic small-caps bold 20px/3 \"Hobo\""
      -- These should not compile:
      -- , renderProperties [font (aFont initial ["Lubalin Graph Medium"] [fantasy])] 
      --     `shouldEqual` "should not compile"
      -- , renderProperties [font (aFont (px 20) [initial] [fantasy])] 
      --     `shouldEqual` "should not compile"
      -- , renderProperties [font (aFont (px 20) initial [fantasy])] 
      --     `shouldEqual` "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Lubalin Graph Medium"] [initial])] 
      --     `shouldEqual` "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Lubalin Graph Medium"] initial)] 
      --     `shouldEqual` "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Hobo"] [] |> withWeight initial)]
      --       `shouldEqual`  "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Hobo"] [] |> withWeight normal)]
      --       `shouldEqual`  "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Hobo"] [] |> withVariant initial)]
      --       `shouldEqual`  "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Hobo"] [] |> withVariant normal)]
      --       `shouldEqual`  "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Hobo"] [] |> withStyle initial)]
      --       `shouldEqual`  "should not compile"
      -- , renderProperties [font (aFont (px 20) ["Hobo"] [] |> withStyle normal)]
      --       `shouldEqual`  "should not compile"
      ]      
    , it "will accept common properties initial, inherit, unset and other"
      [ renderProperties [font initial] `shouldEqual` "font:initial"
      , renderProperties [font inherit] `shouldEqual` "font:inherit" 
      , renderProperties [font unset] `shouldEqual` "font:unset"          
      , renderProperties [font (other "hbs")] `shouldEqual` "font:hbs" 
      , renderProperties [font (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "font:-webkit-foo;font:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [font all]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font auto]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font baseline]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font center]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font normal]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font none]
      --     `shouldEqual` "font:should not compile"      
      -- , renderProperties [font visible]
      --     `shouldEqual` "font:should not compile"
      -- , renderProperties [font hidden]
      --     `shouldEqual` "font:should not compile"
      ]
    ]
  , describe "the letter spacing function"
    [ it "should render a numeric spacing properly"
      [ renderProperties [ letterSpacing (px 20)]
          `shouldEqual` "letter-spacing:20px"
      , renderProperties [letterSpacing (em 0.2)]
          `shouldEqual` "letter-spacing:0.2em"
      ]
    , it "should render generic properties properly"
      [ renderProperties [letterSpacing initial]
          `shouldEqual` "letter-spacing:initial"
      , renderProperties [letterSpacing inherit]
          `shouldEqual` "letter-spacing:inherit"
      , renderProperties [letterSpacing normal]
          `shouldEqual` "letter-spacing:normal"
      , renderProperties [letterSpacing unset]
          `shouldEqual` "letter-spacing:unset"
      , renderProperties [letterSpacing (other "foo")]
          `shouldEqual` "letter-spacing:foo"
      , renderProperties [letterSpacing (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "letter-spacing:-webkit-foo;letter-spacing:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [letterSpacing auto]
      --     `shouldEqual` "letter-spacing:should not compile"
      -- , renderProperties [letterSpacing all]
      --     `shouldEqual` "letter-spacing:should not compile"
      -- , renderProperties [letterSpacing baseline]
      --     `shouldEqual` "letter-spacing:should not compile"
      -- , renderProperties [letterSpacing center]
      --     `shouldEqual` "letter-spacing:should not compile"
      -- , renderProperties [letterSpacing none]
      --     `shouldEqual` "letter-spacing:should not compile"      
      -- , renderProperties [letterSpacing visible]
      --     `shouldEqual` "letter-spacing:should not compile"
      -- , renderProperties [letterSpacing hidden]
      --     `shouldEqual` "letter-spacing:should not compile"
      ]
    ]
  , describe "the word spacing functions"
    [ it "should render a numeric spacing properly"
      [ renderProperties [ wordSpacing (px 20)]
          `shouldEqual` "word-spacing:20px"
      , renderProperties [ wordSpacing (pct 20)]
          `shouldEqual` "word-spacing:20%"
      ]
    , it "should render generic properties properly"
      [ renderProperties [wordSpacing initial]
          `shouldEqual` "word-spacing:initial"
      , renderProperties [wordSpacing inherit]
          `shouldEqual` "word-spacing:inherit"
      , renderProperties [wordSpacing normal]
          `shouldEqual` "word-spacing:normal"
      , renderProperties [wordSpacing unset]
          `shouldEqual` "word-spacing:unset"
      , renderProperties [wordSpacing (other "foo")]
          `shouldEqual` "word-spacing:foo"
      , renderProperties [wordSpacing (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "word-spacing:-webkit-foo;word-spacing:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [wordSpacing auto]
      --     `shouldEqual` "word-spacing:should not compile"
      -- , renderProperties [wordSpacing all]
      --     `shouldEqual` "word-spacing:should not compile"
      -- , renderProperties [wordSpacing baseline]
      --     `shouldEqual` "word-spacing:should not compile"
      -- , renderProperties [wordSpacing center]
      --     `shouldEqual` "word-spacing:should not compile"
      -- , renderProperties [wordSpacing none]
      --     `shouldEqual` "word-spacing:should not compile"      
      -- , renderProperties [wordSpacing visible]
      --     `shouldEqual` "word-spacing:should not compile"
      -- , renderProperties [wordSpacing hidden]
      --     `shouldEqual` "word-spacing:should not compile"
      ] 
    ]
  , describe "the text rendering function"
    [ it "should render properly"
      [ renderProperties [ textRendering optimizeSpeed ]
          `shouldEqual` "text-rendering:optimizeSpeed"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textRendering auto]
          `shouldEqual` "text-rendering:auto"
      , renderProperties [textRendering initial]
          `shouldEqual` "text-rendering:initial"
      , renderProperties [textRendering inherit]
          `shouldEqual` "text-rendering:inherit"
      , renderProperties [textRendering unset]
          `shouldEqual` "text-rendering:unset"
      , renderProperties [textRendering (other "foo")]
          `shouldEqual` "text-rendering:foo"
      , renderProperties [textRendering (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-rendering:-webkit-foo;text-rendering:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textRendering all]
      --     `shouldEqual` "text-rendering:should not compile"
      -- , renderProperties [textRendering baseline]
      --     `shouldEqual` "text-rendering:should not compile"
      -- , renderProperties [textRendering center]
      --     `shouldEqual` "text-rendering:should not compile"
      -- , renderProperties [textRendering normal]
      --     `shouldEqual` "text-rendering:should not compile"
      -- , renderProperties [textRendering none]
      --     `shouldEqual` "text-rendering:should not compile"      
      -- , renderProperties [textRendering visible]
      --     `shouldEqual` "text-rendering:should not compile"
      -- , renderProperties [textRendering hidden]
      --     `shouldEqual` "text-rendering:should not compile"
      ]
    ]
  , describe "the text shadow function"
    [ it "should render a simple shadow properly"
      [ renderProperties [ textShadow (aShadow (px 20) (em 30)) ]
          `shouldEqual` "text-shadow:20px 30em"
      ]
    , it "should render a composite shadows properly"
      [ renderProperties 
        [ textShadow (aShadow (px 20) (em 30) |> shadowBlur (px 30) |> shadowColor green) ]
            `shouldEqual` "text-shadow:20px 30em 30px #73D216"
      , renderProperties 
        [ textShadow (aShadow (px 20) (em 30) |> shadowColor currentColor) ]
            `shouldEqual` "text-shadow:20px 30em currentColor"
      -- , renderProperties 
      --   [ textShadow (aShadow (px 20) (em 30) |> shadowColor inherit) ]
      --       `shouldEqual` "text-shadow:should not compile"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textShadow initial]
          `shouldEqual` "text-shadow:initial"
      , renderProperties [textShadow inherit]
          `shouldEqual` "text-shadow:inherit"
      , renderProperties [textShadow none]
          `shouldEqual` "text-shadow:none"      
      , renderProperties [textShadow unset]
          `shouldEqual` "text-shadow:unset"
      , renderProperties [textShadow (other "foo")]
          `shouldEqual` "text-shadow:foo"
      , renderProperties [textShadow (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-shadow:-webkit-foo;text-shadow:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textShadow auto]
      --     `shouldEqual` "text-shadow:should not compile"
      -- , renderProperties [textShadow all]
      --     `shouldEqual` "text-shadow:should not compile"
      -- , renderProperties [textShadow baseline]
      --     `shouldEqual` "text-shadow:should not compile"
      -- , renderProperties [textShadow center]
      --     `shouldEqual` "text-shadow:should not compile"
      -- , renderProperties [textShadow normal]
      --     `shouldEqual` "text-shadow:should not compile"
      -- , renderProperties [textShadow visible]
      --     `shouldEqual` "text-shadow:should not compile"
      -- , renderProperties [textShadow hidden]
      --     `shouldEqual` "text-shadow:should not compile"
      ]
    ]
  , describe "the text shadows function"
    [ it "should render simple shadows properly"
      [ renderProperties 
          [ textShadows [ (aShadow (px 20) (em 30)), 
                          (aShadow (px 10) (pct 40)) ] ]
              `shouldEqual` "text-shadow:20px 30em,10px 40%"
      ]
    , it "should render composite shadows properly"
      [ renderProperties 
        [ textShadows [ (aShadow (px 20) (em 30) |> shadowBlur (px 30)), 
                        (aShadow (px 10) (pct 40)) ] ]
            `shouldEqual` "text-shadow:20px 30em 30px,10px 40%"
      ]
    ]
  , describe "the text indent function"
    [ it "should render preset values properly"
      [ renderProperties [ textIndent eachLine ]
          `shouldEqual` "text-indent:each-line"
      ]
    , it "should render size values properly"
      [ renderProperties [textIndent (px 30 |> indent)]
          `shouldEqual` "text-indent:30px"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textIndent initial]
          `shouldEqual` "text-indent:initial"
      , renderProperties [textIndent inherit]
          `shouldEqual` "text-indent:inherit"
      , renderProperties [textIndent unset]
          `shouldEqual` "text-indent:unset"
      , renderProperties [textIndent (other "foo")]
          `shouldEqual` "text-indent:foo"
      , renderProperties [textIndent (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-indent:-webkit-foo;text-indent:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textIndent all]
      --     `shouldEqual` "text-indent:should not compile"
      -- , renderProperties [textIndent auto]
      --     `shouldEqual` "text-indent:should not compile"
      -- , renderProperties [textIndent baseline]
      --     `shouldEqual` "text-indent:should not compile"
      -- , renderProperties [textIndent center]
      --     `shouldEqual` "text-indent:should not compile"
      -- , renderProperties [textIndent normal]
      --     `shouldEqual` "text-indent:should not compile"
      -- , renderProperties [textIndent none]
      --     `shouldEqual` "text-indent:should not compile"      
      -- , renderProperties [textIndent visible]
      --     `shouldEqual` "text-indent:should not compile"
      ]
    ]
  , describe "the text direction function"
    [ it "should render preset values properly"
      [ renderProperties [ direction rtl ]
          `shouldEqual` "direction:rtl"
      ]
    , it "should render generic properties properly"
      [ renderProperties [direction initial]
          `shouldEqual` "direction:initial"
      , renderProperties [direction inherit]
          `shouldEqual` "direction:inherit"
      , renderProperties [direction unset]
          `shouldEqual` "direction:unset"
      , renderProperties [direction (other "foo")]
          `shouldEqual` "direction:foo"
      , renderProperties [direction (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "direction:-webkit-foo;direction:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [direction all]
      --     `shouldEqual` "direction:should not compile"
      -- , renderProperties [direction auto]
      --     `shouldEqual` "direction:auto"
      -- , renderProperties [direction baseline]
      --     `shouldEqual` "direction:should not compile"
      -- , renderProperties [direction center]
      --     `shouldEqual` "direction:should not compile"
      -- , renderProperties [direction normal]
      --     `shouldEqual` "direction:should not compile"
      -- , renderProperties [direction none]
      --     `shouldEqual` "direction:should not compile"      
      -- , renderProperties [direction visible]
      --     `shouldEqual` "direction:should not compile"
      -- , renderProperties [direction hidden]
      --     `shouldEqual` "direction:should not compile"
      ]
    ]
  , describe "the text align function"
    [ it "should render preset values properly"
      [ renderProperties [ textAlign start ]
          `shouldEqual` "text-align:start"
      ]
    , it "should render alignment to sides properly"
      [ renderProperties [ textAlign (alignSide sideLeft) ]
          `shouldEqual` "text-align:left"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textAlign initial]
          `shouldEqual` "text-align:initial"
      , renderProperties [textAlign inherit]
          `shouldEqual` "text-align:inherit"
      , renderProperties [textAlign unset]
          `shouldEqual` "text-align:unset"
      , renderProperties [textAlign (other "foo")]
          `shouldEqual` "text-align:foo"
      , renderProperties [textAlign (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-align:-webkit-foo;text-align:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textAlign all]
      --     `shouldEqual` "text-align:should not compile"
      -- , renderProperties [textAlign auto]
      --     `shouldEqual` "text-align:should not compile"
      -- , renderProperties [textAlign baseline]
      --     `shouldEqual` "text-align:should not compile"
      -- , renderProperties [textAlign center]
      --     `shouldEqual` "text-align:should not compile"
      -- , renderProperties [textAlign normal]
      --     `shouldEqual` "text-align:should not compile"
      -- , renderProperties [textAlign none]
      --     `shouldEqual` "text-align:should not compile"      
      -- , renderProperties [textAlign visible]
      --     `shouldEqual` "text-align:should not compile"
      -- , renderProperties [textAlign hidden]
      --     `shouldEqual` "text-align:should not compile"
      ]
    ]
  , describe "the whitespace function"
    [ it "should render preset values properly"
      [ renderProperties [ whiteSpace nowrap ]
          `shouldEqual` "white-space:nowrap"
      ]
    , it "should render generic properties properly"
      [ renderProperties [whiteSpace initial]
          `shouldEqual` "white-space:initial"
      , renderProperties [whiteSpace inherit]
          `shouldEqual` "white-space:inherit"
      , renderProperties [whiteSpace normal]
          `shouldEqual` "white-space:normal"
      , renderProperties [whiteSpace unset]
          `shouldEqual` "white-space:unset"
      , renderProperties [whiteSpace (other "foo")]
          `shouldEqual` "white-space:foo"
      , renderProperties [whiteSpace (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "white-space:-webkit-foo;white-space:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [whiteSpace all]
      --     `shouldEqual` "white-space:should not compile"
      -- , renderProperties [whiteSpace auto]
      --     `shouldEqual` "white-space:should not compile"
      -- , renderProperties [whiteSpace baseline]
      --     `shouldEqual` "white-space:should not compile"
      -- , renderProperties [whiteSpace center]
      --     `shouldEqual` "white-space:should not compile"
      -- , renderProperties [whiteSpace none]
      --     `shouldEqual` "white-space:should not compile"      
      -- , renderProperties [whiteSpace visible]
      --     `shouldEqual` "white-space:should not compile"
      -- , renderProperties [whiteSpace hidden]
      --     `shouldEqual` "white-space:should not compile"
      ]
    ]
-- TODO There are more complex values for decoration, e.g., 
-- text-decoration: underline red; 
-- text-decoration: underline wavy red;
  , describe "the text decoration function"
    [ it "should render preset values properly"
      [ renderProperties [ textDecoration underline ]
          `shouldEqual` "text-decoration:underline"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textDecoration initial]
          `shouldEqual` "text-decoration:initial"
      , renderProperties [textDecoration inherit]
          `shouldEqual` "text-decoration:inherit"
      , renderProperties [textDecoration none]
          `shouldEqual` "text-decoration:none"
      , renderProperties [textDecoration unset]
          `shouldEqual` "text-decoration:unset"
      , renderProperties [textDecoration (other "foo")]
          `shouldEqual` "text-decoration:foo"
      , renderProperties [textDecoration (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-decoration:-webkit-foo;text-decoration:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textDecoration all]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecoration auto]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecoration baseline]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecoration center]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecoration normal]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecoration visible]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecoration hidden]
      --     `shouldEqual` "text-decoration-line:should not compile"
      ]
    ]
-- TODO There are more complex values for text-decoration-line, e.g., 
-- text-decoration: underline overline; 
-- text-decoration: overline underline line-through;
  , describe "the text decoration line function"
    [ it "should render preset values properly"
      [ renderProperties [ textDecorationLine overline ]
          `shouldEqual` "text-decoration-line:overline"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textDecorationLine initial]
          `shouldEqual` "text-decoration-line:initial"
      , renderProperties [textDecorationLine inherit]
          `shouldEqual` "text-decoration-line:inherit"
      , renderProperties [textDecorationLine none]
          `shouldEqual` "text-decoration-line:none"
      , renderProperties [textDecorationLine unset]
          `shouldEqual` "text-decoration-line:unset"
      , renderProperties [textDecorationLine (other "foo")]
          `shouldEqual` "text-decoration-line:foo"
      , renderProperties [textDecorationLine (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-decoration-line:-webkit-foo;text-decoration-line:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textDecorationLine all]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecorationLine auto]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecorationLine baseline]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecorationLine center]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecorationLine normal]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecorationLine visible]
      --     `shouldEqual` "text-decoration-line:should not compile"
      -- , renderProperties [textDecorationLine hidden]
      --     `shouldEqual` "text-decoration-line:should not compile"
      ]
    ]
  , describe "the text decoration color function"
    [ it "should render preset values properly"
      [ renderProperties [ textDecorationColor green ]
          `shouldEqual` "text-decoration-color:#73D216"
      , renderProperties [ textDecorationColor (rgb 255 255 255) ]
          `shouldEqual` "text-decoration-color:#FFFFFF"
      , renderProperties [ textDecorationColor currentColor ]
          `shouldEqual` "text-decoration-color:currentColor"
      , renderProperties [ textDecorationColor transparent ]
          `shouldEqual` "text-decoration-color:transparent"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textDecorationColor initial]
          `shouldEqual` "text-decoration-color:initial"
      , renderProperties [textDecorationColor inherit]
          `shouldEqual` "text-decoration-color:inherit"
      , renderProperties [textDecorationColor unset]
          `shouldEqual` "text-decoration-color:unset"
      , renderProperties [textDecorationColor (other "foo")]
          `shouldEqual` "text-decoration-color:foo"
      , renderProperties [textDecorationColor (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-decoration-color:-webkit-foo;text-decoration-color:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textDecorationColor all]
      --     `shouldEqual` "text-decoration-color:should not compile"
      -- , renderProperties [textDecorationColor auto]
      --     `shouldEqual` "text-decoration-color:should not compile"
      -- , renderProperties [textDecorationColor baseline]
      --     `shouldEqual` "text-decoration-color:should not compile"
      -- , renderProperties [textDecorationColor center]
      --     `shouldEqual` "text-decoration-color:should not compile"
      -- , renderProperties [textDecorationColor none]
      --     `shouldEqual` "text-decoration-color:should not compile"
      -- , renderProperties [textDecorationColor normal]
      --     `shouldEqual` "text-decoration-color:should not compile"
      -- , renderProperties [textDecorationColor visible]
      --     `shouldEqual` "text-decoration-color:should not compile"
      -- , renderProperties [textDecorationColor hidden]
      --     `shouldEqual` "text-decoration-color:should not compile"
      ]
    ]
  , describe "the text decoration style function"
    [ it "should render preset values properly"
      [ renderProperties [ textDecorationStyle solid ]
          `shouldEqual` "text-decoration-style:solid"
      , renderProperties [ textDecorationStyle double ]
          `shouldEqual` "text-decoration-style:double"
      , renderProperties [ textDecorationStyle dashed ]
          `shouldEqual` "text-decoration-style:double"
      , renderProperties [ textDecorationStyle wavy ]
          `shouldEqual` "text-decoration-style:double"
      -- , renderProperties [ textDecorationStyle groove ]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [ textDecorationStyle ridge ]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [ textDecorationStyle inset ]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [ textDecorationStyle outset ]
      --     `shouldEqual` "text-decoration-style:should not compile"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textDecorationStyle initial]
          `shouldEqual` "text-decoration-style:initial"
      , renderProperties [textDecorationStyle inherit]
          `shouldEqual` "text-decoration-style:inherit"
      , renderProperties [textDecorationStyle unset]
          `shouldEqual` "text-decoration-style:unset"
      , renderProperties [textDecorationStyle (other "foo")]
          `shouldEqual` "text-decoration-style:foo"
      , renderProperties [textDecorationStyle (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-decoration-style:-webkit-foo;text-decoration-style:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textDecorationStyle all]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [textDecorationStyle auto]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [textDecorationStyle baseline]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [textDecorationStyle center]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [textDecorationStyle none]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [textDecorationStyle normal]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [textDecorationStyle visible]
      --     `shouldEqual` "text-decoration-style:should not compile"
      -- , renderProperties [textDecorationStyle hidden]
      --     `shouldEqual` "text-decoration-style:should not compile"
      ]
    ]
  , describe "the text transform function"
    [ it "should render preset properties properly"
      [ renderProperties [ textTransform lowercase ]
          `shouldEqual` "text-transform:lowercase"
      ]
    , it "should render generic properties properly"
      [ renderProperties [textTransform initial]
          `shouldEqual` "text-transform:initial"
      , renderProperties [textTransform inherit]
          `shouldEqual` "text-transform:inherit"
      , renderProperties [textTransform none]
          `shouldEqual` "text-transform:none"      
      , renderProperties [textTransform unset]
          `shouldEqual` "text-transform:unset"
      , renderProperties [textTransform (other "foo")]
          `shouldEqual` "text-transform:foo"
      , renderProperties [textTransform (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "text-transform:-webkit-foo;text-transform:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [textTransform all]
      --     `shouldEqual` "text-transform:should not compile"
      -- , renderProperties [textTransform auto]
      --     `shouldEqual` "text-transform:should not compile"
      -- , renderProperties [textTransform baseline]
      --     `shouldEqual` "text-transform:should not compile"
      -- , renderProperties [textTransform center]
      --     `shouldEqual` "text-transform:should not compile"
      -- , renderProperties [textTransform normal]
      --     `shouldEqual` "text-transform:should not compile"
      -- , renderProperties [textTransform visible]
      --     `shouldEqual` "text-transform:should not compile"
      -- , renderProperties [textTransform hidden]
      --     `shouldEqual` "text-transform:should not compile"
      ]
    ]
-- TODO need textOverflow, textJustify   
-- TODO list-style-type can take a string argument
  , describe "The list style type function"
    [ it "should render a named style type properly"
      [ renderProperties [listStyleType disc]
          `shouldEqual` "list-style-type:disc"
      ]
    , it "should render generic style types properly"
      [ renderProperties [listStyleType none]
          `shouldEqual` "list-style-type:none"
      , renderProperties [listStyleType inherit]
          `shouldEqual` "list-style-type:inherit"
      , renderProperties [listStyleType initial]
          `shouldEqual` "list-style-type:initial"
      , renderProperties [listStyleType unset]
          `shouldEqual` "list-style-type:unset"
      , renderProperties [listStyleType (other "foo")]
          `shouldEqual` "list-style-type:foo"
      , renderProperties [listStyleType (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "list-style-type:-webkit-foo;list-style-type:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [listStyleType all]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType auto]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType baseline]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType center]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType normal]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType visible]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType hidden]
      --     `shouldEqual` "list-style-type:should not compile"
      ]  
    ]
  , describe "The list style position function"
    [ it "should render a named style position properly"
      [ renderProperties [listStylePosition inside]
          `shouldEqual` "list-style-position:inside"
      ]
    , it "should render generic style positions properly"
      [  renderProperties [listStylePosition inherit]
          `shouldEqual` "list-style-position:inherit"
      , renderProperties [listStylePosition initial]
          `shouldEqual` "list-style-position:initial"
      , renderProperties [listStylePosition unset]
          `shouldEqual` "list-style-position:unset"
      , renderProperties [listStylePosition (other "foo")]
          `shouldEqual` "list-style-position:foo"
      , renderProperties [listStylePosition (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "list-style-position:-webkit-foo;list-style-position:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [listStylePosition all]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition auto]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition baseline]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition center]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition none]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition normal]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition visible]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition hidden]
      --     `shouldEqual` "list-style-position:should not compile"
      ]  
    ]
  , describe "The list style image function"
    [ it "should render a named style image properly"
      [ renderProperties [listStyleImage (imageUrl "http://www.foo.com")]
          `shouldEqual` "list-style-image:url(\"http://www.foo.com\")"
      ]
    , it "should render generic style images properly"
      [  renderProperties [listStyleImage inherit]
          `shouldEqual` "list-style-image:inherit"
      , renderProperties [listStyleImage initial]
          `shouldEqual` "list-style-image:initial"
      , renderProperties [listStyleImage none]
          `shouldEqual` "list-style-image:none"
      , renderProperties [listStyleImage unset]
          `shouldEqual` "list-style-image:unset"
      , renderProperties [listStyleImage (other "foo")]
          `shouldEqual` "list-style-image:foo"
      , renderProperties [listStyleImage (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "list-style-image:-webkit-foo;list-style-image:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [listStyleImage all]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage auto]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage baseline]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage center]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage normal]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage visible]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage hidden]
      --     `shouldEqual` "list-style-image:should not compile"
      ]  
    ]  
  , describe "The list style shorthand function"
    [ it "should render combinations of styles properly"
      [ renderProperties 
          [ listStyle (withListImage (imageUrl "http://www.foo.com")) ]
            `shouldEqual` "list-style:url(\"http://www.foo.com\")"
      , renderProperties 
          [ listStyle (withListType disc >> withListPosition inside) ]
            `shouldEqual` "list-style:disc inside"
      ]
    , it "should render generic styles properly"
      [  renderProperties [ listStyle inherit ]
          `shouldEqual` "list-style:inherit"
      , renderProperties [ listStyle initial ]
          `shouldEqual` "list-style:initial"
      , renderProperties [ listStyle unset ]
          `shouldEqual` "list-style:unset"
      , renderProperties [ listStyle (other "foo") ]
          `shouldEqual` "list-style:foo"
      , renderProperties [listStyle (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "list-style:-webkit-foo;list-style:-moz-foo" 
      -- Should not compile:
      -- , renderProperties [listStyle all]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle auto]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle baseline]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle center]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle none]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle normal]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle visible]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle hidden]
      --     `shouldEqual` "list-style:should not compile"
      ]  
    ]  
  ]
