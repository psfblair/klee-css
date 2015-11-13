module Css.GeometryTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Common exposing (..)
import Css.Geometry exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.GeometryTests"
-- TODO Test something that takes angles
  [ describe "The positioning functions"
    [ it "should render the top property correctly"
      [ renderProperties [top (px 20)]
          `shouldEqual` "top:20px"
      , renderProperties [top (pct 20)]
          `shouldEqual` "top:20%"
      , renderProperties [top nil]
          `shouldEqual` "top:0"
      ]
    , it "should render generic arguments to the top property correctly"
      [ renderProperties [top auto]
          `shouldEqual` "top:auto"
      , renderProperties [top initial]
          `shouldEqual` "top:initial"
      , renderProperties [top inherit]
          `shouldEqual` "top:inherit"
      , renderProperties [top unset]
          `shouldEqual` "top:unset"
      , renderProperties [top (other "foo")]
          `shouldEqual` "top:foo"
      , renderProperties [top (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "top:-webkit-foo;top:-moz-foo"
      -- , renderProperties [top all]
      --     `shouldEqual` "top:should not compile"
      -- , renderProperties [top baseline]
      --     `shouldEqual` "top:should not compile"
      -- , renderProperties [top center]
      --     `shouldEqual` "top:should not compile"
      -- , renderProperties [top normal]
      --     `shouldEqual` "top:should not compile"
      -- , renderProperties [top none]
      --     `shouldEqual` "top:should not compile"
      -- , renderProperties [top visible]
      --     `shouldEqual` "top:should not compile"
      -- , renderProperties [top hidden]
      --     `shouldEqual` "top:should not compile"
      ]      
    , it "should render the right property correctly"
      [ renderProperties [right (px 20)]
          `shouldEqual` "right:20px"
      ]
    , it "should render generic arguments to the right property correctly"
      [ renderProperties [right auto]
          `shouldEqual` "right:auto"
      , renderProperties [right initial]
          `shouldEqual` "right:initial"
      , renderProperties [right inherit]
          `shouldEqual` "right:inherit"
      , renderProperties [right unset]
          `shouldEqual` "right:unset"
      , renderProperties [right (other "foo")]
          `shouldEqual` "right:foo"
      , renderProperties [right (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "right:-webkit-foo;right:-moz-foo"
      -- , renderProperties [right all]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right baseline]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right center]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right normal]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right none]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right visible]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [bottom hidden]
      --     `shouldEqual` "bottom:should not compile"
      ]      
    , it "should render the left property correctly"
      [ renderProperties [left (px 20)]
          `shouldEqual` "left:20px"
      ]
    , it "should render generic arguments to the left property correctly"
      [ renderProperties [left auto]
          `shouldEqual` "left:auto"
      , renderProperties [left initial]
          `shouldEqual` "left:initial"
      , renderProperties [left inherit]
          `shouldEqual` "left:inherit"
      , renderProperties [left unset]
          `shouldEqual` "left:unset"
      , renderProperties [left (other "foo")]
          `shouldEqual` "left:foo"
      , renderProperties [left (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "left:-webkit-foo;left:-moz-foo"
      -- , renderProperties [left all]
      --     `shouldEqual` "left:should not compile"
      -- , renderProperties [left baseline]
      --     `shouldEqual` "left:should not compile"
      -- , renderProperties [left center]
      --     `shouldEqual` "left:should not compile"
      -- , renderProperties [left normal]
      --     `shouldEqual` "left:should not compile"
      -- , renderProperties [left none]
      --     `shouldEqual` "left:should not compile"
      -- , renderProperties [left visible]
      --     `shouldEqual` "left:should not compile"
      -- , renderProperties [left hidden]
      --     `shouldEqual` "left:should not compile"
      ]        
    , it "should render the right property correctly"
      [ renderProperties [right (px 20)]
          `shouldEqual` "right:20px"
      ]
    , it "should render generic arguments to the right property correctly"
      [ renderProperties [right auto]
          `shouldEqual` "right:auto"
      , renderProperties [right initial]
          `shouldEqual` "right:initial"
      , renderProperties [right inherit]
          `shouldEqual` "right:inherit"
      , renderProperties [right unset]
          `shouldEqual` "right:unset"
      , renderProperties [right (other "foo")]
          `shouldEqual` "right:foo"
      , renderProperties [right (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "right:-webkit-foo;right:-moz-foo"
      -- , renderProperties [right all]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right baseline]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right center]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right normal]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right none]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right visible]
      --     `shouldEqual` "right:should not compile"
      -- , renderProperties [right hidden]
      --     `shouldEqual` "right:should not compile"
      ]  
    ]
  , describe "The sizing functions"
    [ it "should render the width property correctly"
      [ renderProperties [width (px 20)]
          `shouldEqual` "width:20px"
      , renderProperties [width (pct 20)]
          `shouldEqual` "width:20%"
      ]
    , it "should render generic arguments to the width property correctly"
      [ renderProperties [width auto]
          `shouldEqual` "width:auto"
      , renderProperties [width initial]
          `shouldEqual` "width:initial"
      , renderProperties [width inherit]
          `shouldEqual` "width:inherit"
      , renderProperties [width unset]
          `shouldEqual` "width:unset"
      , renderProperties [width (other "foo")]
          `shouldEqual` "width:foo"
      , renderProperties [width (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "width:-webkit-foo;width:-moz-foo"
      -- , renderProperties [width all]
      --     `shouldEqual` "width:should not compile"
      -- , renderProperties [width baseline]
      --     `shouldEqual` "width:should not compile"
      -- , renderProperties [width center]
      --     `shouldEqual` "width:should not compile"
      -- , renderProperties [width normal]
      --     `shouldEqual` "width:should not compile"
      -- , renderProperties [width none]
      --     `shouldEqual` "width:should not compile"
      -- , renderProperties [width visible]
      --     `shouldEqual` "width:should not compile"
      -- , renderProperties [width hidden]
      --     `shouldEqual` "width:should not compile"
      ]        
    , it "should render the height property correctly"
      [ renderProperties [height (px 20)]
          `shouldEqual` "height:20px"
      , renderProperties [height (pct 20)]
          `shouldEqual` "height:20%"
      ]
    , it "should render generic arguments to the height property correctly"
      [ renderProperties [height auto]
          `shouldEqual` "height:auto"
      , renderProperties [height initial]
          `shouldEqual` "height:initial"
      , renderProperties [height inherit]
          `shouldEqual` "height:inherit"
      , renderProperties [height unset]
          `shouldEqual` "height:unset"
      , renderProperties [height (other "foo")]
          `shouldEqual` "height:foo"
      , renderProperties [height (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "height:-webkit-foo;height:-moz-foo"
      -- , renderProperties [height all]
      --     `shouldEqual` "height:should not compile"
      -- , renderProperties [height baseline]
      --     `shouldEqual` "height:should not compile"
      -- , renderProperties [height center]
      --     `shouldEqual` "height:should not compile"
      -- , renderProperties [height normal]
      --     `shouldEqual` "height:should not compile"
      -- , renderProperties [height none]
      --     `shouldEqual` "height:should not compile"
      -- , renderProperties [height visible]
      --     `shouldEqual` "height:should not compile"
      -- , renderProperties [height hidden]
      --     `shouldEqual` "height:should not compile"
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
    , it "should render the margin-right property correctly"
      [ renderProperties [marginRight (px 20)]
          `shouldEqual` "margin-right:20px"
      , renderProperties [marginRight (pct 20)]
          `shouldEqual` "margin-right:20%"
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
