module Css.LayoutTests () where

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
      [ renderProperties [boxSizing paddingBox] 
          `shouldEqual` "box-sizing:padding-box"
      , renderProperties [boxSizing borderBox] 
          `shouldEqual` "box-sizing:border-box"
      , renderProperties [boxSizing contentBox] 
          `shouldEqual` "box-sizing:content-box"
      ]
    , it "should render the generic box sizing properties properly"
      [ renderProperties [boxSizing initial] `shouldEqual` "box-sizing:initial"
      , renderProperties [boxSizing inherit] `shouldEqual` "box-sizing:inherit"
      , renderProperties [boxSizing unset] `shouldEqual` "box-sizing:unset"
      , renderProperties [boxSizing (other "foo")] 
          `shouldEqual` "box-sizing:foo"
      , renderProperties [boxSizing (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` "box-sizing:-webkit-foo;box-sizing:-moz-foo"
      -- , renderProperties [boxSizing all]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing auto]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing baseline]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing center]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing normal]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing none]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing visible]
      --     `shouldEqual` "box-sizing:should not compile"
      -- , renderProperties [boxSizing hidden]
      --     `shouldEqual` "box-sizing:should not compile"
      ]
    ]
  , describe "The "
    [ it "should render "
      [ renderProperties [ ] 
          `shouldEqual` " "
      , renderProperties [ ] 
          `shouldEqual` " "
      ]
    , it "should render the generic "
      [ renderProperties [ initial] `shouldEqual` ":initial"
      , renderProperties [ inherit] `shouldEqual` ":inherit"
      , renderProperties [ unset] `shouldEqual` " :unset"
      , renderProperties [ (other "foo")] 
          `shouldEqual` ":foo"
      , renderProperties [ (otherPrefixed [webkit_, moz_] "foo")] 
          `shouldEqual` ":-webkit-foo;:-moz-foo"
      -- , renderProperties [ all]
      --     `shouldEqual` ":should not compile"
      -- , renderProperties [ auto]
      --     `shouldEqual` ":should not compile"
      -- , renderProperties [ baseline]
      --     `shouldEqual` ":should not compile"
      -- , renderProperties [ center]
      --     `shouldEqual` ":should not compile"
      -- , renderProperties [ normal]
      --     `shouldEqual` ":should not compile"
      -- , renderProperties [ none]
      --     `shouldEqual` ":should not compile"
      -- , renderProperties [ visible]
      --     `shouldEqual` ":should not compile"
      -- , renderProperties [ hidden]
      --     `shouldEqual` ":should not compile"
      ]
    ]
    , describe "The "
      [ it "should render "
        [ renderProperties [ ] 
            `shouldEqual` " "
        , renderProperties [ ] 
            `shouldEqual` " "
        ]
      , it "should render the generic "
        [ renderProperties [ initial] `shouldEqual` ":initial"
        , renderProperties [ inherit] `shouldEqual` ":inherit"
        , renderProperties [ unset] `shouldEqual` " :unset"
        , renderProperties [ (other "foo")] 
            `shouldEqual` ":foo"
        , renderProperties [ (otherPrefixed [webkit_, moz_] "foo")] 
            `shouldEqual` ":-webkit-foo;:-moz-foo"
        -- , renderProperties [ all]
        --     `shouldEqual` ":should not compile"
        -- , renderProperties [ auto]
        --     `shouldEqual` ":should not compile"
        -- , renderProperties [ baseline]
        --     `shouldEqual` ":should not compile"
        -- , renderProperties [ center]
        --     `shouldEqual` ":should not compile"
        -- , renderProperties [ normal]
        --     `shouldEqual` ":should not compile"
        -- , renderProperties [ none]
        --     `shouldEqual` ":should not compile"
        -- , renderProperties [ visible]
        --     `shouldEqual` ":should not compile"
        -- , renderProperties [ hidden]
        --     `shouldEqual` ":should not compile"
        ]
      ]
    ]
      
