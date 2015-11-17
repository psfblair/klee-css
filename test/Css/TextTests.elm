module Css.TextTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.ColorsAndStrokes exposing 
  (green, currentColor, transparent, rgb, solid, disc)
import Css.Common exposing (..)
import Css.Geometry exposing (..)
import Css.Text exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.TextTests"
  [ describe "the letter spacing functios"
    [ it "should render a numeric spacing properly"
      [ renderProperties [ letterSpacing (px 20)]
          `shouldEqual` "letter-spacing:20px"
      ]
-- TODO
{-
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
-}
    ]
  , describe "the word spacing functions"
    [ it "should render a numeric spacing properly"
      [ renderProperties [ wordSpacing (pct 20)]
          `shouldEqual` "word-spacing:20%"
      ]
-- TODO 
{-
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
-}
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
      --   [ textShadow (aShadow (px 20) (em 30) |> shadowColor transparent) ]
      --       `shouldEqual` "text-shadow:should not compile"
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
          [ textShadows [ (aShadow (px 20) (em 30)), (aShadow (px 10) (pct 40)) ] ]
              `shouldEqual` "text-shadow:20px 30em,10px 40%"
      ]
    , it "should render omposite shadows properly"
      [ renderProperties 
        [ textShadows [(aShadow (px 20) (em 30) |> shadowBlur (px 30)), (aShadow (px 10) (pct 40)) ] ]
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
      -- , renderProperties [textIndent hidden]
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
-- TODO text-decoration can have neither, only solid double dotted dashed wavy
-- cannot have the rest of the strokes used in borders        
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
  , describe "the text decoration line function"
    [ it "should render preset values properly"
      [ renderProperties [ textDecorationLine underline ]
          `shouldEqual` "text-decoration-line:underline"
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
      ]
-- TODO
{-
    , it "should render generic properties properly"
      [ renderProperties [textDecorationStyle initial]
          `shouldEqual` "text-decoration-style:initial"
      , renderProperties [textDecorationStyle inherit]
          `shouldEqual` "text-decoration-style:inherit"
      -- , renderProperties [textDecorationStyle unset]
      --     `shouldEqual` "text-decoration-style:unset"
      , renderProperties [textDecorationStyle (other "foo")]
          `shouldEqual` "text-decoration-style:foo"
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
-}
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
      , renderProperties [listStyleType (other "foo")]
          `shouldEqual` "list-style-type:foo"
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
      -- , renderProperties [listStyleType unset]
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
      , renderProperties [listStylePosition (other "foo")]
          `shouldEqual` "list-style-position:foo"
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
      -- , renderProperties [listStylePosition unset]
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
      , renderProperties [listStyleImage (other "foo")]
          `shouldEqual` "list-style-image:foo"
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
      -- , renderProperties [listStyleImage unset]
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
      , renderProperties [ listStyle (other "foo") ]
          `shouldEqual` "list-style:foo"
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
      -- , renderProperties [listStyle unset]
      --     `shouldEqual` "list-style:should not compile"
      ]  
    ]  
  ]
