module Css.GeometryTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Geometry exposing (..)
import Css.Size exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.GeometryTests"
  [ describe "The positioning functions"
    [ it "should render the size property correctly"
      [ renderProperties [size (px 20)]
          `shouldEqual` "size:20px"
      , renderProperties [size (pct 20)]
          `shouldEqual` "size:20%"
      ]
    , it "should render the top property correctly"
      [ renderProperties [top (px 20)]
          `shouldEqual` "top:20px"
      , renderProperties [top (pct 20)]
          `shouldEqual` "top:20%"
      ]
    , it "should render the bottom property correctly"
      [ renderProperties [bottom (px 20)]
          `shouldEqual` "bottom:20px"
      , renderProperties [bottom (pct 20)]
          `shouldEqual` "bottom:20%"
      ]
    , it "should render the left property correctly"
      [ renderProperties [left (px 20)]
          `shouldEqual` "left:20px"
      , renderProperties [left (pct 20)]
          `shouldEqual` "left:20%"
      ]
    , it "should render the right property correctly"
      [ renderProperties [right (px 20)]
          `shouldEqual` "right:20px"
      , renderProperties [right (pct 20)]
          `shouldEqual` "right:20%"
      ]
    ]
  , describe "The sizing functions"
    [ it "should render the width property correctly"
      [ renderProperties [width (px 20)]
          `shouldEqual` "width:20px"
      , renderProperties [width (pct 20)]
          `shouldEqual` "width:20%"
      ]
    , it "should render the height property correctly"
      [ renderProperties [height (px 20)]
          `shouldEqual` "height:20px"
      , renderProperties [height (pct 20)]
          `shouldEqual` "height:20%"
      ]
    , it "should render the min-width property correctly"
      [ renderProperties [minWidth (px 20)]
          `shouldEqual` "min-width:20px"
      , renderProperties [minWidth (pct 20)]
          `shouldEqual` "min-width:20%"
      ]
    , it "should render the min-height property correctly"
      [ renderProperties [minHeight (px 20)]
          `shouldEqual` "min-height:20px"
      , renderProperties [minHeight (pct 20)]
          `shouldEqual` "min-height:20%"
      ]
    , it "should render the max-width property correctly"
      [ renderProperties [maxWidth (px 20)]
          `shouldEqual` "max-width:20px"
      , renderProperties [maxWidth (pct 20)]
          `shouldEqual` "max-width:20%"
      ]
    , it "should render the max-height property correctly"
      [ renderProperties [maxHeight (px 20)]
          `shouldEqual` "max-height:20px"
      , renderProperties [maxHeight (pct 20)]
          `shouldEqual` "max-height:20%"
      ]
    ]
  , describe "The padding functions"
    [ it "should render the padding property correctly"
      [ renderProperties [padding (px 20) (px 30) (px 40) (px 50)]
          `shouldEqual` "padding:20px 30px 40px 50px"
      , renderProperties [padding (pct 20) (pct 30) (pct 40) (pct 50)]
          `shouldEqual` "padding:20% 30% 40% 50%"
      ]
    , it "should render the padding-top property correctly"
      [ renderProperties [paddingTop (px 20)]
          `shouldEqual` "padding-top:20px"
      , renderProperties [paddingTop (pct 20)]
          `shouldEqual` "padding-top:20%"
      ]
    , it "should render the padding-bottom property correctly"
      [ renderProperties [paddingBottom (px 20)]
          `shouldEqual` "padding-bottom:20px"
      , renderProperties [paddingBottom (pct 20)]
          `shouldEqual` "padding-bottom:20%"
      ]
    , it "should render the padding-left property correctly"
      [ renderProperties [paddingLeft (px 20)]
          `shouldEqual` "padding-left:20px"
      , renderProperties [paddingLeft (pct 20)]
          `shouldEqual` "padding-left:20%"
      ]
    , it "should render the padding-right property correctly"
      [ renderProperties [paddingRight (px 20)]
          `shouldEqual` "padding-right:20px"
      , renderProperties [paddingRight (pct 20)]
          `shouldEqual` "padding-right:20%"
      ]
    ]
  , describe "The margin functions"
    [ it "should render the margin property correctly"
      [ renderProperties [margin (px 20) (px 30) (px 40) (px 50)]
          `shouldEqual` "margin:20px 30px 40px 50px"
      , renderProperties [margin (pct 20) (pct 30) (pct 40) (pct 50)]
          `shouldEqual` "margin:20% 30% 40% 50%"
      ]
    , it "should render the margin-top property correctly"
      [ renderProperties [marginTop (px 20)]
          `shouldEqual` "margin-top:20px"
      , renderProperties [marginTop (pct 20)]
          `shouldEqual` "margin-top:20%"
      ]
    , it "should render the margin-bottom property correctly"
      [ renderProperties [marginBottom (px 20)]
          `shouldEqual` "margin-bottom:20px"
      , renderProperties [marginBottom (pct 20)]
          `shouldEqual` "margin-bottom:20%"
      ]
    , it "should render the margin-left property correctly"
      [ renderProperties [marginLeft (px 20)]
          `shouldEqual` "margin-left:20px"
      , renderProperties [marginLeft (pct 20)]
          `shouldEqual` "margin-left:20%"
      ]
    , it "should render the margin-right property correctly"
      [ renderProperties [marginRight (px 20)]
          `shouldEqual` "margin-right:20px"
      , renderProperties [marginRight (pct 20)]
          `shouldEqual` "margin-right:20%"
      ]
    ]
  ]
