module Css.LayoutTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Box exposing (..)
import Css.ColorsAndStrokes exposing (..)
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
      , renderProperties [ clear unset   ] `shouldEqual` "clear:unset"
      , renderProperties [ clear (other "foo")] 
          `shouldEqual` "clear:foo"
      , renderProperties [ clear (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "clear:-webkit-foo;clear:-moz-foo"
      -- , renderProperties [ clear all]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear auto]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear baseline]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear center]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear normal]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear none]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear visible]
      --     `shouldEqual` "clear:should not compile"
      -- , renderProperties [ clear hidden]
      --     `shouldEqual` "clear:should not compile"
      ]
    ]
    -- TODO The clip property is obsolete; replace with clip-path.
    , describe "The clip function"
      [ it "should render "
        [ renderProperties [ clip (rect (px 20) (px 30) (px 40) (px 50)) ] 
            `shouldEqual` "clip:rect(20px,30px,40px,50px)"
        ]
      , it "should render the generic "
        [ renderProperties [ clip initial ] `shouldEqual` "clip:initial"
        , renderProperties [ clip inherit ] `shouldEqual` "clip:inherit"
        , renderProperties [ clip auto    ] `shouldEqual` "clip:auto"
        , renderProperties [ clip unset   ] `shouldEqual` "clip:unset"
        , renderProperties [ clip (other "foo") ] 
            `shouldEqual` "clip:foo"
        , renderProperties [ clip (otherPrefixed [webkit_, moz_] "foo")] 
            `shouldEqual` "clip:-webkit-foo;clip:-moz-foo"
        -- , renderProperties [ clip all]
        --     `shouldEqual` "clip:should not compile"
        -- , renderProperties [ clip baseline]
        --     `shouldEqual` "clip:should not compile"
        -- , renderProperties [ clip center]
        --     `shouldEqual` "clip:should not compile"
        -- , renderProperties [ clip normal]
        --     `shouldEqual` "clip:should not compile"
        -- , renderProperties [ clip none]
        --     `shouldEqual` "clip:should not compile"
        -- , renderProperties [ clip visible]
        --     `shouldEqual` "clip:should not compile"
        -- , renderProperties [ clip hidden]
        --     `shouldEqual` "clip:should not compile"
        ]
      ]
    ]
      
