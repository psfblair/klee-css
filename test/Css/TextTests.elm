module Css.TextTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Border exposing (solid)
import Css.Color exposing (green)
import Css.Common exposing (..)
import Css.List exposing (disc)
import Css.Internal.Position exposing (sideLeft)
import Css.Size exposing (..)
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
      ]
    , it "should render generic properties properly"
      [ renderProperties [textDecorationColor initial]
          `shouldEqual` "text-decoration-color:initial"
      , renderProperties [textDecorationColor inherit]
          `shouldEqual` "text-decoration-color:inherit"
-- TODO
      -- , renderProperties [textDecorationColor unset]
      --     `shouldEqual` "text-decoration-color:unset"
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
  , describe "the content function"
    [ it "should render the various types of content properly"
      [ renderProperties [ content openQuote ]
          `shouldEqual` "content:open-quote"
      , renderProperties [ content (stringContent "prefix") ]
          `shouldEqual` "content:\"prefix\""
      , renderProperties [ content (attrContent "href") ]
          `shouldEqual` "content:attr(href)"
      , renderProperties [ content (urlContent "http://www.foo.com") ]
          `shouldEqual` "content:url(http://www.foo.com)"
      ]
    , it "should render counters properly"
      [ renderProperties [ content (counter "countername") ]
          `shouldEqual` "content:counter(countername)"
      , renderProperties [ content (styledCounter "countername" disc) ]
          `shouldEqual` "content:counter(countername,disc)"
      , renderProperties [ content (counters "countername" ".") ]
          `shouldEqual` "content:counters(countername,\".\")"
      , renderProperties [ content (styledCounters "countername" "." disc) ]
          `shouldEqual` "content:counters(countername,\".\",disc)"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ content initial ]
          `shouldEqual` "content:initial"
      , renderProperties [ content inherit ]
          `shouldEqual` "content:inherit"
      , renderProperties [ content none ]
          `shouldEqual` "content:none"      
      , renderProperties [ content normal ]
          `shouldEqual` "content:normal"
      , renderProperties [ content unset ]
          `shouldEqual` "content:unset"
      , renderProperties [ content (other "foo") ]
          `shouldEqual` "content:foo"
      -- Should not compile:
      -- , renderProperties [content all]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content auto]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content baseline]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content center]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content visible]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content hidden]
      --     `shouldEqual` "content:should not compile"
      ]
    ]
  , describe "the counter increment function"
    [ it "should render a single counter increment"
      [ renderProperties [ counterIncrement (counterId "fred") ]
          `shouldEqual` "counter-increment:fred"
      , renderProperties [ counterIncrement (withStep "fred" -2) ]
          `shouldEqual` "counter-increment:fred -2"
      ]
    , it "should render multiple counter increments"
      [ renderProperties 
          [ counterIncrements [ (counterId "fred"), (withStep "wilma" -2) ] ]
            `shouldEqual` "counter-increment:fred wilma -2"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ counterIncrement initial ]
          `shouldEqual` "counter-increment:initial"
      , renderProperties [ counterIncrement inherit ]
          `shouldEqual` "counter-increment:inherit"
      , renderProperties [ counterIncrement none ]
          `shouldEqual` "counter-increment:none"      
      , renderProperties [ counterIncrement unset ]
          `shouldEqual` "counter-increment:unset"
      , renderProperties [ counterIncrement (other "foo") ]
          `shouldEqual` "counter-increment:foo"
      -- Should not compile:
      -- , renderProperties [counterIncrement all]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement auto]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement baseline]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement center]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [ counterIncrement normal ]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement visible]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement hidden]
      --     `shouldEqual` "counter-increment:should not compile"
      ]
    ]
  , describe "the counter reset functions"
    [ it "should render a single counter reset"
      [ renderProperties [ counterReset (counterId "fred") ]
          `shouldEqual` "counter-reset:fred"
      , renderProperties [ counterReset (resetTo "fred" -2) ]
          `shouldEqual` "counter-reset:fred -2"
      ]
    , it "should render multiple counter resets"
      [ renderProperties 
          [ counterResets [ (counterId "fred"), (resetTo "wilma" -2) ] ]
            `shouldEqual` "counter-reset:fred wilma -2"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ counterReset initial ]
          `shouldEqual` "counter-reset:initial"
      , renderProperties [ counterReset inherit ]
          `shouldEqual` "counter-reset:inherit"
      , renderProperties [ counterReset none ]
          `shouldEqual` "counter-reset:none"      
      , renderProperties [ counterReset unset ]
          `shouldEqual` "counter-reset:unset"
      , renderProperties [ counterReset (other "foo") ]
          `shouldEqual` "counter-reset:foo"
      -- Should not compile:
      -- , renderProperties [counterReset all]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset auto]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset baseline]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset center]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [ counterReset normal ]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset visible]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset hidden]
      --     `shouldEqual` "counter-reset:should not compile"
      ]
    ]
  ]
