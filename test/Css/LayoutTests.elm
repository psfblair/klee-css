module Css.LayoutTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Common exposing (..)
import Css.Layout exposing (..)
import Css.Geometry exposing (..)
import Css exposing (renderProperties)

suite : Spec
suite = describe "Css.LayoutTests"
  [ describe "The box sizing function"
    [ it "should render the box sizing properties properly"
      [ renderProperties [ boxSizing paddingBox ] 
          `shouldEqual` "box-sizing:padding-box"
      , renderProperties [ boxSizing borderBox ] 
          `shouldEqual` "box-sizing:border-box"
      , renderProperties [ boxSizing contentBox ] 
          `shouldEqual` "box-sizing:content-box"
      ]
    , it "should render the generic box sizing properties properly"
      [ renderProperties [ boxSizing initial ] `shouldEqual` "box-sizing:initial"
      , renderProperties [ boxSizing inherit ] `shouldEqual` "box-sizing:inherit"
      , renderProperties [ boxSizing unset ] `shouldEqual` "box-sizing:unset"
      , renderProperties [ boxSizing (other "foo") ] 
          `shouldEqual` "box-sizing:foo"
      , renderProperties [ boxSizing (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "box-sizing:-webkit-foo;box-sizing:-moz-foo"
      -- , renderProperties [ boxSizing all ]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [ boxSizing auto ]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [ boxSizing baseline ]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [ boxSizing center ]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [ boxSizing normal ]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [ boxSizing none ]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [ boxSizing visible ]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [ boxSizing hidden ]
      --     `shouldEqual` "box-sizing:should not compile"
      ]
    ]
  , describe "The clear function"
    [ it "should render specific values properly"
      [ renderProperties [ clear clearLeft ] 
          `shouldEqual` "clear:left"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ clear initial ] `shouldEqual` "clear:initial"
      , renderProperties [ clear inherit ] `shouldEqual` "clear:inherit"
      , renderProperties [ clear none    ] `shouldEqual` "clear:none"
      , renderProperties [ clear unset   ] `shouldEqual` "clear:unset"
      , renderProperties [ clear (other "foo") ] 
          `shouldEqual` "clear:foo"
      , renderProperties [ clear (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "clear:-webkit-foo;clear:-moz-foo"
      -- , renderProperties [ clear all ]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear auto ]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear baseline ]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear center ]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear normal ]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear visible ]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear hidden ]
      --     `shouldEqual` "clear:should not compile"
      ]
    ]
    -- TODO The clip property is obsolete; replace with clip-path.
  , describe "The clip function"
    [ it "should render a rect"
      [ renderProperties [ clip (rect (px 20) (px 30) (px 40) (px 50)) ] 
          `shouldEqual` "clip:rect(20px,30px,40px,50px)"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ clip initial ] `shouldEqual` "clip:initial"
      , renderProperties [ clip inherit ] `shouldEqual` "clip:inherit"
      , renderProperties [ clip auto    ] `shouldEqual` "clip:auto"
      , renderProperties [ clip unset   ] `shouldEqual` "clip:unset"
      , renderProperties [ clip (other "foo") ] 
          `shouldEqual` "clip:foo"
      , renderProperties [ clip (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "clip:-webkit-foo;clip:-moz-foo"
      -- , renderProperties [ clip all ]
      --     `shouldEqual` "clip:should not compile"
      -- , renderProperties [ clip baseline ]
      --     `shouldEqual` "clip:should not compile"
      -- , renderProperties [ clip center ]
      --     `shouldEqual` "clip:should not compile"
      -- , renderProperties [ clip normal ]
      --     `shouldEqual` "clip:should not compile"
      -- , renderProperties [ clip none ]
      --     `shouldEqual` "clip:should not compile"
      -- , renderProperties [ clip visible ]
      --     `shouldEqual` "clip:should not compile"
      -- , renderProperties [ clip hidden ]
      --     `shouldEqual` "clip:should not compile"
      ]
    ]
  , describe "The display function"
    [ it "should render specific values properly"
      [ renderProperties [ display inline ] 
          `shouldEqual` "display:inline"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ display initial ] `shouldEqual` "display:initial"
      , renderProperties [ display inherit ] `shouldEqual` "display:inherit"
      , renderProperties [ display none    ] `shouldEqual` "display:none"
      , renderProperties [ display unset   ] `shouldEqual` "display:unset"
      , renderProperties [ display (other "foo") ] 
          `shouldEqual` "display:foo"
      , renderProperties [ display (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "display:-webkit-foo;display:-moz-foo"
      -- , renderProperties [ display all ]
      --     `shouldEqual` "display:should not compile"
      -- , renderProperties [ display auto ]
      --     `shouldEqual` "display:should not compile"
      -- , renderProperties [ display baseline ]
      --     `shouldEqual` "display:should not compile"
      -- , renderProperties [ display center ]
      --     `shouldEqual` "display:should not compile"
      -- , renderProperties [ display normal ]
      --     `shouldEqual` "display:should not compile"
      -- , renderProperties [ display visible ]
      --     `shouldEqual` "display:should not compile"
      -- , renderProperties [ display hidden ]
      --     `shouldEqual` "display:should not compile"
      ]
    ]
  , describe "The float function"
    [ it "should render specific values properly"
      [ renderProperties [ float floatLeft ] 
          `shouldEqual` "float:left"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ float initial ] `shouldEqual` "float:initial"
      , renderProperties [ float inherit ] `shouldEqual` "float:inherit"
      , renderProperties [ float none    ] `shouldEqual` "float:none"
      , renderProperties [ float unset   ] `shouldEqual` "float:unset"
      , renderProperties [ float (other "foo") ] 
          `shouldEqual` "float:foo"
      , renderProperties [ float (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "float:-webkit-foo;float:-moz-foo"
      -- , renderProperties [ float all ]
      --     `shouldEqual` "float:should not compile"
      -- , renderProperties [ float auto ]
      --     `shouldEqual` "float:should not compile"
      -- , renderProperties [ float baseline ]
      --     `shouldEqual` "float:should not compile"
      -- , renderProperties [ float center ]
      --     `shouldEqual` "float:should not compile"
      -- , renderProperties [ float normal ]
      --     `shouldEqual` "float:should not compile"
      -- , renderProperties [ float visible ]
      --     `shouldEqual` "float:should not compile"
      -- , renderProperties [ float hidden ]
      --     `shouldEqual` "float:should not compile"
      ]
    ]
  , describe "The opacity function"
    [ it "should render specific values properly"
      [ renderProperties [ opacity (opacityLevel 0.6) ] 
          `shouldEqual` "opacity:0.6"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ opacity initial ] `shouldEqual` "opacity:initial"
      , renderProperties [ opacity inherit ] `shouldEqual` "opacity:inherit"
      , renderProperties [ opacity unset   ] `shouldEqual` "opacity:unset"
      , renderProperties [ opacity (other "foo") ] 
          `shouldEqual` "opacity:foo"
      , renderProperties [ opacity (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "opacity:-webkit-foo;opacity:-moz-foo"
      -- , renderProperties [ opacity all ]
      --     `shouldEqual` "opacity:should not compile"
      -- , renderProperties [ opacity auto ]
      --     `shouldEqual` "opacity:should not compile"
      -- , renderProperties [ opacity baseline ]
      --     `shouldEqual` "opacity:should not compile"
      -- , renderProperties [ opacity center ]
      --     `shouldEqual` "opacity:should not compile"
      -- , renderProperties [ opacity none ] 
      --     `shouldEqual` "opacity:should not compile"
      -- , renderProperties [ opacity normal ]
      --     `shouldEqual` "opacity:should not compile"
      -- , renderProperties [ opacity visible ]
      --     `shouldEqual` "opacity:should not compile"
      -- , renderProperties [ opacity hidden ]
      --     `shouldEqual` "opacity:should not compile"
      ]
    ]
  , describe "The overflow function"
    [ it "should render specific values properly"
      [ renderProperties [ overflow scroll ] 
          `shouldEqual` "overflow:scroll"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ overflow initial ] `shouldEqual` "overflow:initial"
      , renderProperties [ overflow inherit ] `shouldEqual` "overflow:inherit"
      , renderProperties [ overflow auto    ] `shouldEqual` "overflow:auto"
      , renderProperties [ overflow visible ] `shouldEqual` "overflow:visible"
      , renderProperties [ overflow hidden  ] `shouldEqual` "overflow:hidden"
      , renderProperties [ overflow unset   ] `shouldEqual` "overflow:unset"
      , renderProperties [ overflow (other "foo") ] 
          `shouldEqual` "overflow:foo"
      , renderProperties [ overflow (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "overflow:-webkit-foo;overflow:-moz-foo"
      -- , renderProperties [ overflow all ]
      --     `shouldEqual` "overflow:should not compile"
      -- , renderProperties [ overflow baseline ]
      --     `shouldEqual` "overflow:should not compile"
      -- , renderProperties [ overflow center ]
      --     `shouldEqual` "overflow:should not compile"
      -- , renderProperties [ overflow none ] 
      --     `shouldEqual` "overflow:should not compile"
      -- , renderProperties [ overflow normal ]
      --     `shouldEqual` "overflow:should not compile"
      ]
    ]
  , describe "The overflowX function"
    [ it "should render specific values properly"
      [ renderProperties [ overflowX scroll ] 
          `shouldEqual` "overflow-x:scroll"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ overflowX initial ] `shouldEqual` "overflow-x:initial"
      , renderProperties [ overflowX inherit ] `shouldEqual` "overflow-x:inherit"
      , renderProperties [ overflowX auto    ] `shouldEqual` "overflow-x:auto"
      , renderProperties [ overflowX visible ] `shouldEqual` "overflow-x:visible"
      , renderProperties [ overflowX hidden  ] `shouldEqual` "overflow-x:hidden"
      , renderProperties [ overflowX unset   ] `shouldEqual` "overflow-x:unset"
      , renderProperties [ overflowX (other "foo") ] 
          `shouldEqual` "overflow-x:foo"
      , renderProperties [ overflowX (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "overflow-x:-webkit-foo;overflow-x:-moz-foo"
      -- , renderProperties [ overflowX all ]
      --     `shouldEqual` "overflow-x:should not compile"
      -- , renderProperties [ overflowX baseline ]
      --     `shouldEqual` "overflow-x:should not compile"
      -- , renderProperties [ overflowX center ]
      --     `shouldEqual` "overflow-x:should not compile"
      -- , renderProperties [ overflowX none ] 
      --     `shouldEqual` "overflow-x:should not compile"
      -- , renderProperties [ overflowX normal ]
      --     `shouldEqual` "overflow-x:should not compile"
      ]
    ]
  , describe "The overflowY function"
    [ it "should render specific values properly"
      [ renderProperties [ overflowY scroll ] 
          `shouldEqual` "overflow-y:scroll"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ overflowY initial ] `shouldEqual` "overflow-y:initial"
      , renderProperties [ overflowY inherit ] `shouldEqual` "overflow-y:inherit"
      , renderProperties [ overflowY auto    ] `shouldEqual` "overflow-y:auto"
      , renderProperties [ overflowY visible ] `shouldEqual` "overflow-y:visible"
      , renderProperties [ overflowY hidden  ] `shouldEqual` "overflow-y:hidden"
      , renderProperties [ overflowY unset   ] `shouldEqual` "overflow-y:unset"
      , renderProperties [ overflowY (other "foo") ] 
          `shouldEqual` "overflow-y:foo"
      , renderProperties [ overflowY (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "overflow-y:-webkit-foo;overflow-y:-moz-foo"
      -- , renderProperties [ overflowY all ]
      --     `shouldEqual` "overflow-y:should not compile"
      -- , renderProperties [ overflowY baseline ]
      --     `shouldEqual` "overflow-y:should not compile"
      -- , renderProperties [ overflowY center ]
      --     `shouldEqual` "overflow-y:should not compile"
      -- , renderProperties [ overflowY none ] 
      --     `shouldEqual` "overflow-y:should not compile"
      -- , renderProperties [ overflowY normal ]
      --     `shouldEqual` "overflow-y:should not compile"
      ]
    ]
  , describe "The position function"
    [ it "should render specific values properly"
      [ renderProperties [ position static ] 
          `shouldEqual` "position:static"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ position initial ] `shouldEqual` "position:initial"
      , renderProperties [ position inherit ] `shouldEqual` "position:inherit"
      , renderProperties [ position unset   ] `shouldEqual` "position:unset"
      , renderProperties [ position (other "foo") ] 
          `shouldEqual` "position:foo"
      , renderProperties [ position (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "position:-webkit-foo;position:-moz-foo"
      -- , renderProperties [ position all ]
      --     `shouldEqual` "position:should not compile"
      -- , renderProperties [ position auto ]
      --     `shouldEqual` "position:should not compile"
      -- , renderProperties [ position baseline ]
      --     `shouldEqual` "position:should not compile"
      -- , renderProperties [ position center ]
      --     `shouldEqual` "position:should not compile"
      -- , renderProperties [ position none ] 
      --     `shouldEqual` "position:should not compile"
      -- , renderProperties [ position normal ]
      --     `shouldEqual` "position:should not compile"
      -- , renderProperties [ position visible ]
      --     `shouldEqual` "position:should not compile"
      -- , renderProperties [ position hidden ]
      --     `shouldEqual` "position:should not compile"
      ]
    ]
  , describe "The verticalAlign function"
    [ it "should render specific values properly"
      [ renderProperties [ verticalAlign vAlignSub ] 
          `shouldEqual` "vertical-align:sub"
      , renderProperties [ verticalAlign (px 20) ] 
          `shouldEqual` "vertical-align:20px"
      , renderProperties [ verticalAlign (pct 20) ] 
          `shouldEqual` "vertical-align:20%"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ verticalAlign initial ] 
          `shouldEqual` "vertical-align:initial"
      , renderProperties [ verticalAlign inherit ] 
          `shouldEqual` "vertical-align:inherit"
      , renderProperties [ verticalAlign baseline ]
          `shouldEqual` "vertical-align:baseline"
      , renderProperties [ verticalAlign unset   ] 
          `shouldEqual` "vertical-align:unset"
      , renderProperties [ verticalAlign (other "foo") ] 
          `shouldEqual` "vertical-align:foo"
      , renderProperties [ verticalAlign (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "vertical-align:-webkit-foo;vertical-align:-moz-foo"
      -- , renderProperties [ verticalAlign all ]
      --     `shouldEqual` "vertical-align:should not compile"
      -- , renderProperties [ verticalAlign auto ]
      --     `shouldEqual` "vertical-align:should not compile"
      -- , renderProperties [ verticalAlign center ]
      --     `shouldEqual` "vertical-align:should not compile"
      -- , renderProperties [ verticalAlign none ] 
      --     `shouldEqual` "vertical-align:should not compile"
      -- , renderProperties [ verticalAlign normal ]
      --     `shouldEqual` "vertical-align:should not compile"
      -- , renderProperties [ verticalAlign visible ]
      --     `shouldEqual` "vertical-align:should not compile"
      -- , renderProperties [ verticalAlign hidden ]
      --     `shouldEqual` "vertical-align:should not compile"
      ]
    ]
  , describe "The visibility function"
    [ it "should render specific values properly"
      [ renderProperties [ visibility collapse ] 
          `shouldEqual` "visibility:collapse"
      -- , renderProperties [ visibility separate ] 
      --     `shouldEqual` "visibility:should not compile - only for border-collapse"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ visibility initial ] `shouldEqual` "visibility:initial"
      , renderProperties [ visibility inherit ] `shouldEqual` "visibility:inherit"
      , renderProperties [ visibility visible ] `shouldEqual` "visibility:visible"
      , renderProperties [ visibility hidden  ] `shouldEqual` "visibility:hidden"
      , renderProperties [ visibility unset   ] `shouldEqual` "visibility:unset"
      , renderProperties [ visibility (other "foo") ] 
          `shouldEqual` "visibility:foo"
      , renderProperties [ visibility (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "visibility:-webkit-foo;visibility:-moz-foo"
      -- , renderProperties [ visibility all ]
      --     `shouldEqual` "visibility:should not compile"
      -- , renderProperties [ visibility auto ]
      --     `shouldEqual` "visibility:should not compile"
      -- , renderProperties [ visibility baseline ]
      --     `shouldEqual` "visibility:should not compile"
      -- , renderProperties [ visibility center ]
      --     `shouldEqual` "visibility:should not compile"
      -- , renderProperties [ visibility none ] 
      --     `shouldEqual` "visibility:should not compile"
      -- , renderProperties [ visibility normal ]
      --     `shouldEqual` "visibility:should not compile"
      ]
    ]
  , describe "The zIndex function"
    [ it "should render specific values properly"
      [ renderProperties [ zIndex (zLevel 100) ] 
          `shouldEqual` "z-index:100"
      , renderProperties [ zIndex (zLevel -1) ] 
          `shouldEqual` "z-index:-1"
      ]
    , it "should render the generic properties properly"
      [ renderProperties [ zIndex initial ] `shouldEqual` "z-index:initial"
      , renderProperties [ zIndex inherit ] `shouldEqual` "z-index:inherit"
      , renderProperties [ zIndex auto    ] `shouldEqual` "z-index:auto"      
      , renderProperties [ zIndex unset   ] `shouldEqual` "z-index:unset"
      , renderProperties [ zIndex (other "foo") ] 
          `shouldEqual` "z-index:foo"
      , renderProperties [ zIndex (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "z-index:-webkit-foo;z-index:-moz-foo"
      -- , renderProperties [ zIndex all ]
      --     `shouldEqual` "z-index:should not compile"
      -- , renderProperties [ zIndex baseline ]
      --     `shouldEqual` "z-index:should not compile"
      -- , renderProperties [ zIndex center ]
      --     `shouldEqual` "z-index:should not compile"
      -- , renderProperties [ zIndex none ] 
      --     `shouldEqual` "z-index:should not compile"
      -- , renderProperties [ zIndex normal ]
      --     `shouldEqual` "z-index:should not compile"
      -- , renderProperties [ zIndex visible ] 
      --     `shouldEqual` "z-index:should not compile"
      -- , renderProperties [ zIndex hidden  ] 
      --     `shouldEqual` "z-index:should not compile"      
      ]
    ]
  ]
