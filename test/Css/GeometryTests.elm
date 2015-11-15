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
      , renderProperties [top abs0]
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
      [ renderProperties [left (pct 20)]
          `shouldEqual` "left:20%"
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
    , it "should render the bottom property correctly"
      [ renderProperties [bottom (px 20)]
          `shouldEqual` "bottom:20px"
      ]
    , it "should render generic arguments to the bottom property correctly"
      [ renderProperties [bottom auto]
          `shouldEqual` "bottom:auto"
      , renderProperties [bottom initial]
          `shouldEqual` "bottom:initial"
      , renderProperties [bottom inherit]
          `shouldEqual` "bottom:inherit"
      , renderProperties [bottom unset]
          `shouldEqual` "bottom:unset"
      , renderProperties [bottom (other "foo")]
          `shouldEqual` "bottom:foo"
      , renderProperties [bottom (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "bottom:-webkit-foo;bottom:-moz-foo"
      -- , renderProperties [bottom all]
      --     `shouldEqual` "bottom:should not compile"
      -- , renderProperties [bottom baseline]
      --     `shouldEqual` "bottom:should not compile"
      -- , renderProperties [bottom center]
      --     `shouldEqual` "bottom:should not compile"
      -- , renderProperties [bottom normal]
      --     `shouldEqual` "bottom:should not compile"
      -- , renderProperties [bottom none]
      --     `shouldEqual` "bottom:should not compile"
      -- , renderProperties [bottom visible]
      --     `shouldEqual` "bottom:should not compile"
      -- , renderProperties [bottom hidden]
      --     `shouldEqual` "bottom:should not compile"
      ]  
    ]
  , describe "The sizing functions"
    [ it "should render the width property correctly"
      [ renderProperties [width (pct 20)]
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
      [ renderProperties [minWidth (pct 20)]
          `shouldEqual` "min-width:20%"
      ]
    , it "should render generic arguments to the min-width property correctly"
      [ renderProperties [minWidth initial]
          `shouldEqual` "min-width:initial"
      , renderProperties [minWidth inherit]
          `shouldEqual` "min-width:inherit"
      , renderProperties [minWidth unset]
          `shouldEqual` "min-width:unset"
      , renderProperties [minWidth (other "foo")]
          `shouldEqual` "min-width:foo"
      , renderProperties [minWidth (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "min-width:-webkit-foo;min-width:-moz-foo"
      -- , renderProperties [minWidth all]
      --     `shouldEqual` "min-width:should not compile"
      -- , renderProperties [minWidth auto]
      --     `shouldEqual` "min-width:should not compile"
      -- , renderProperties [minWidth baseline]
      --     `shouldEqual` "min-width:should not compile"
      -- , renderProperties [minWidth center]
      --     `shouldEqual` "min-width:should not compile"
      -- , renderProperties [minWidth normal]
      --     `shouldEqual` "min-width:should not compile"
      -- , renderProperties [minWidth none]
      --     `shouldEqual` "min-width:should not compile"
      -- , renderProperties [minWidth visible]
      --     `shouldEqual` "min-width:should not compile"
      -- , renderProperties [minWidth hidden]
      --     `shouldEqual` "min-width:should not compile"
      ] 
    , it "should render the min-height property correctly"
      [ renderProperties [minHeight (px 20)]
          `shouldEqual` "min-height:20px"
      ]
    , it "should render generic arguments to the min-height property correctly"
      [ renderProperties [minHeight initial]
          `shouldEqual` "min-height:initial"
      , renderProperties [minHeight inherit]
          `shouldEqual` "min-height:inherit"
      , renderProperties [minHeight unset]
          `shouldEqual` "min-height:unset"
      , renderProperties [minHeight (other "foo")]
          `shouldEqual` "min-height:foo"
      , renderProperties [minHeight (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "min-height:-webkit-foo;min-height:-moz-foo"
      -- , renderProperties [minHeight all]
      --     `shouldEqual` "min-height:should not compile"
      -- , renderProperties [minHeight auto]
      --     `shouldEqual` "min-height:should not compile"
      -- , renderProperties [minHeight baseline]
      --     `shouldEqual` "min-height:should not compile"
      -- , renderProperties [minHeight center]
      --     `shouldEqual` "min-height:should not compile"
      -- , renderProperties [minHeight normal]
      --     `shouldEqual` "min-height:should not compile"
      -- , renderProperties [minHeight none]
      --     `shouldEqual` "min-height:should not compile"
      -- , renderProperties [minHeight visible]
      --     `shouldEqual` "min-height:should not compile"
      -- , renderProperties [minHeight hidden]
      --     `shouldEqual` "min-height:should not compile"
      ] 
    , it "should render the max-width property correctly"
      [ renderProperties [maxWidth (pct 20)]
          `shouldEqual` "max-width:20%"
      ]
    , it "should render generic arguments to the max-width property correctly"
      [ renderProperties [maxWidth initial]
          `shouldEqual` "max-width:initial"
      , renderProperties [maxWidth inherit]
          `shouldEqual` "max-width:inherit"
      , renderProperties [maxWidth unset]
          `shouldEqual` "max-width:unset"
      , renderProperties [maxWidth none]
          `shouldEqual` "max-width:none"          
      , renderProperties [maxWidth (other "foo")]
          `shouldEqual` "max-width:foo"
      , renderProperties [maxWidth (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "max-width:-webkit-foo;max-width:-moz-foo"
      -- , renderProperties [maxWidth all]
      --     `shouldEqual` "max-width:should not compile"
      -- , renderProperties [maxWidth auto]
      --     `shouldEqual` "max-width:should not compile"
      -- , renderProperties [maxWidth baseline]
      --     `shouldEqual` "max-width:should not compile"
      -- , renderProperties [maxWidth center]
      --     `shouldEqual` "max-width:should not compile"
      -- , renderProperties [maxWidth normal]
      --     `shouldEqual` "max-width:should not compile"
      -- , renderProperties [maxWidth visible]
      --     `shouldEqual` "max-width:should not compile"
      -- , renderProperties [maxWidth hidden]
      --     `shouldEqual` "max-width:should not compile"
      ]
    , it "should render the max-height property correctly"
      [ renderProperties [maxHeight (px 20)]
          `shouldEqual` "max-height:20px"
      ]
    , it "should render generic arguments to the max-height property correctly"
      [ renderProperties [maxHeight initial]
          `shouldEqual` "max-height:initial"
      , renderProperties [maxHeight inherit]
          `shouldEqual` "max-height:inherit"
      , renderProperties [maxHeight none]
          `shouldEqual` "max-height:none"
      , renderProperties [maxHeight unset]
          `shouldEqual` "max-height:unset"
      , renderProperties [maxHeight (other "foo")]
          `shouldEqual` "max-height:foo"
      , renderProperties [maxHeight (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "max-height:-webkit-foo;max-height:-moz-foo"
      -- , renderProperties [maxHeight all]
      --     `shouldEqual` "max-height:should not compile"
      -- , renderProperties [maxHeight auto]
      --     `shouldEqual` "max-height:should not compile"
      -- , renderProperties [maxHeight baseline]
      --     `shouldEqual` "max-height:should not compile"
      -- , renderProperties [maxHeight center]
      --     `shouldEqual` "max-height:should not compile"
      -- , renderProperties [maxHeight normal]
      --     `shouldEqual` "max-height:should not compile"
      -- , renderProperties [maxHeight visible]
      --     `shouldEqual` "max-height:should not compile"
      -- , renderProperties [maxHeight hidden]
      --     `shouldEqual` "max-height:should not compile"
      ] 
    ]
  , describe "The padding functions"
    [ it "should render the padding property correctly"
      [ renderProperties [padding (rect (px 20) (px 30) (px 40) (px 50))]
          `shouldEqual` "padding:20px 30px 40px 50px"
      , renderProperties [padding (rect (pct 20) (pct 30) (pct 40) (pct 50))]
          `shouldEqual` "padding:20% 30% 40% 50%"
      , renderProperties [padding (rect abs0 (px 30) (px 40) abs0)]
          `shouldEqual` "padding:0 30px 40px 0"
      , renderProperties [padding (rect (pct 20) rel0 (pct 40) (pct 50))]
          `shouldEqual` "padding:20% 0 40% 50%"
      -- , renderProperties [padding (rect (px 20) (em 30) (em 40) (em 50))]
      --     `shouldEqual` "padding:will not compile"
      -- , renderProperties [padding (rect (em 20) (px 30) (em 40) (em 50))]
      --     `shouldEqual` "padding:will not compile"
      -- , renderProperties [padding (rect abs0 (em 30) (em 40) (em 50))]
      --     `shouldEqual` "padding:will not compile"
      -- , renderProperties [padding (rect (px 20) (px 30) rel0 (px 50))]
      --     `shouldEqual` "padding:will not compile"
      ]
    , it "should render generic arguments to the padding property correctly"
      [ renderProperties [padding initial]
          `shouldEqual` "padding:initial"
      , renderProperties [padding inherit]
          `shouldEqual` "padding:inherit"
      , renderProperties [padding unset]
          `shouldEqual` "padding:unset"
      , renderProperties [padding (other "foo")]
          `shouldEqual` "padding:foo"
      , renderProperties [padding (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "padding:-webkit-foo;padding:-moz-foo"
      -- , renderProperties [padding (rect initial (px 30) (px 40) (px 50))]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding (rect (px 30) initial (px 40) (px 50))]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding (rect (px 30) (px 40) initial (px 50))]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding (rect (px 30) (px 40) (px 50) initial)]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding all]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding auto]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding baseline]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding center]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding none]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding normal]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding visible]
      --     `shouldEqual` "padding:should not compile"
      -- , renderProperties [padding hidden]
      --     `shouldEqual` "padding:should not compile"
      ] 
    , it "should render the padding-top property correctly"
      [ renderProperties [paddingTop (px 20)]
          `shouldEqual` "padding-top:20px"
      , renderProperties [paddingTop (pct 20)]
          `shouldEqual` "padding-top:20%"
      ]
    , it "should render generic arguments to the padding-top property correctly"
      [ renderProperties [paddingTop initial]
          `shouldEqual` "padding-top:initial"
      , renderProperties [paddingTop inherit]
          `shouldEqual` "padding-top:inherit"
      , renderProperties [paddingTop unset]
          `shouldEqual` "padding-top:unset"
      , renderProperties [paddingTop (other "foo")]
          `shouldEqual` "padding-top:foo"
      , renderProperties [paddingTop (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "padding-top:-webkit-foo;padding-top:-moz-foo"
      -- , renderProperties [paddingTop all]
      --     `shouldEqual` "padding-top:should not compile"
      -- , renderProperties [paddingTop auto]
      --     `shouldEqual` "padding-top:should not compile"
      -- , renderProperties [paddingTop baseline]
      --     `shouldEqual` "padding-top:should not compile"
      -- , renderProperties [paddingTop center]
      --     `shouldEqual` "padding-top:should not compile"
      -- , renderProperties [paddingTop none]
      --     `shouldEqual` "padding-top:should not compile"          
      -- , renderProperties [paddingTop normal]
      --     `shouldEqual` "padding-top:should not compile"
      -- , renderProperties [paddingTop visible]
      --     `shouldEqual` "padding-top:should not compile"
      -- , renderProperties [paddingTop hidden]
      --     `shouldEqual` "padding-top:should not compile"
      ]
    , it "should render the padding-bottom property correctly"
      [ renderProperties [paddingBottom (px 20)]
          `shouldEqual` "padding-bottom:20px"
      , renderProperties [paddingBottom (pct 20)]
          `shouldEqual` "padding-bottom:20%"
      ]
    , it "should render generic arguments to the padding-bottom property correctly"
      [ renderProperties [paddingBottom initial]
          `shouldEqual` "padding-bottom:initial"
      , renderProperties [paddingBottom inherit]
          `shouldEqual` "padding-bottom:inherit"
      , renderProperties [paddingBottom unset]
          `shouldEqual` "padding-bottom:unset"
      , renderProperties [paddingBottom (other "foo")]
          `shouldEqual` "padding-bottom:foo"
      , renderProperties [paddingBottom (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "padding-bottom:-webkit-foo;padding-bottom:-moz-foo"
      -- , renderProperties [paddingBottom all]
      --     `shouldEqual` "padding-bottom:should not compile"
      -- , renderProperties [paddingBottom auto]
      --     `shouldEqual` "padding-bottom:should not compile"
      -- , renderProperties [paddingBottom baseline]
      --     `shouldEqual` "padding-bottom:should not compile"
      -- , renderProperties [paddingBottom center]
      --     `shouldEqual` "padding-bottom:should not compile"
      -- , renderProperties [paddingBottom none]
      --     `shouldEqual` "padding-bottom:should not compile"          
      -- , renderProperties [paddingBottom normal]
      --     `shouldEqual` "padding-bottom:should not compile"
      -- , renderProperties [paddingBottom visible]
      --     `shouldEqual` "padding-bottom:should not compile"
      -- , renderProperties [paddingBottom hidden]
      --     `shouldEqual` "padding-bottom:should not compile"
      ]
    , it "should render the padding-left property correctly"
      [ renderProperties [paddingLeft (px 20)]
          `shouldEqual` "padding-left:20px"
      , renderProperties [paddingLeft (pct 20)]
          `shouldEqual` "padding-left:20%"
      ]
    , it "should render generic arguments to the padding-left property correctly"
      [ renderProperties [paddingLeft initial]
          `shouldEqual` "padding-left:initial"
      , renderProperties [paddingLeft inherit]
          `shouldEqual` "padding-left:inherit"
      , renderProperties [paddingLeft unset]
          `shouldEqual` "padding-left:unset"
      , renderProperties [paddingLeft (other "foo")]
          `shouldEqual` "padding-left:foo"
      , renderProperties [paddingLeft (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "padding-left:-webkit-foo;padding-left:-moz-foo"
      -- , renderProperties [paddingLeft all]
      --     `shouldEqual` "padding-left:should not compile"
      -- , renderProperties [paddingLeft auto]
      --     `shouldEqual` "padding-left:should not compile"
      -- , renderProperties [paddingLeft baseline]
      --     `shouldEqual` "padding-left:should not compile"
      -- , renderProperties [paddingLeft center]
      --     `shouldEqual` "padding-left:should not compile"
      -- , renderProperties [paddingLeft none]
      --     `shouldEqual` "padding-left:should not compile"          
      -- , renderProperties [paddingLeft normal]
      --     `shouldEqual` "padding-left:should not compile"
      -- , renderProperties [paddingLeft visible]
      --     `shouldEqual` "padding-left:should not compile"
      -- , renderProperties [paddingLeft hidden]
      --     `shouldEqual` "padding-left:should not compile"
      ]      
    , it "should render the padding-right property correctly"
      [ renderProperties [paddingRight (px 20)]
          `shouldEqual` "padding-right:20px"
      , renderProperties [paddingRight (pct 20)]
          `shouldEqual` "padding-right:20%"
      ]
    , it "should render generic arguments to the padding-right property correctly"
      [ renderProperties [paddingRight initial]
          `shouldEqual` "padding-right:initial"
      , renderProperties [paddingRight inherit]
          `shouldEqual` "padding-right:inherit"
      , renderProperties [paddingRight unset]
          `shouldEqual` "padding-right:unset"
      , renderProperties [paddingRight (other "foo")]
          `shouldEqual` "padding-right:foo"
      , renderProperties [paddingRight (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "padding-right:-webkit-foo;padding-right:-moz-foo"
      -- , renderProperties [paddingRight all]
      --     `shouldEqual` "padding-right:should not compile"
      -- , renderProperties [paddingRight auto]
      --     `shouldEqual` "padding-right:should not compile"
      -- , renderProperties [paddingRight baseline]
      --     `shouldEqual` "padding-right:should not compile"
      -- , renderProperties [paddingRight center]
      --     `shouldEqual` "padding-right:should not compile"
      -- , renderProperties [paddingRight none]
      --     `shouldEqual` "padding-right:should not compile"          
      -- , renderProperties [paddingRight normal]
      --     `shouldEqual` "padding-right:should not compile"
      -- , renderProperties [paddingRight visible]
      --     `shouldEqual` "padding-right:should not compile"
      -- , renderProperties [paddingRight hidden]
      --     `shouldEqual` "padding-right:should not compile"
      ]
    ]
  , describe "The margin functions"
    [ it "should render the margin property correctly"
      [ renderProperties [margin (rect (px 20) (px 30) (px 40) (px 50))]
          `shouldEqual` "margin:20px 30px 40px 50px"
      , renderProperties [margin (rect (pct 20) (pct 30) (pct 40) (pct 50))]
          `shouldEqual` "margin:20% 30% 40% 50%"
      , renderProperties [margin (rect abs0 (px 30) (px 40) abs0)]
          `shouldEqual` "margin:0 30px 40px 0"
      , renderProperties [margin (rect (pct 20) rel0 (pct 40) (pct 50))]
          `shouldEqual` "margin:20% 0 40% 50%"
      -- , renderProperties [margin (rect (px 20) (em 30) (em 40) (em 50))]
      --     `shouldEqual` "margin:will not compile"
      -- , renderProperties [margin (rect (em 20) (px 30) (em 40) (em 50))]
      --     `shouldEqual` "margin:will not compile"
      -- , renderProperties [margin (rect abs0 (em 30) (em 40) (em 50))]
      --     `shouldEqual` "margin:will not compile"
      -- , renderProperties [margin (rect (px 20) (px 30) rel0 (px 50))]
      --     `shouldEqual` "margin:will not compile"
      ]
    , it "should render generic arguments to the margin property correctly"
      [ renderProperties [margin initial]
          `shouldEqual` "margin:initial"
      , renderProperties [margin inherit]
          `shouldEqual` "margin:inherit"
      , renderProperties [margin auto]
          `shouldEqual` "margin:auto"
      , renderProperties [margin unset]
          `shouldEqual` "margin:unset"
      , renderProperties [margin (other "foo")]
          `shouldEqual` "margin:foo"
      , renderProperties [margin (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "margin:-webkit-foo;margin:-moz-foo"
      -- , renderProperties [margin (rect initial (px 30) (px 40) (px 50))]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin (rect (px 30) initial (px 40) (px 50))]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin (rect (px 30) (px 40) initial (px 50))]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin (rect (px 30) (px 40) (px 50) initial)]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin all]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin baseline]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin center]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin none]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin normal]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin visible]
      --     `shouldEqual` "margin:should not compile"
      -- , renderProperties [margin hidden]
      --     `shouldEqual` "margin:should not compile"
      ]       
    , it "should render the margin-top property correctly"
      [ renderProperties [marginTop (px 20)]
          `shouldEqual` "margin-top:20px"
      , renderProperties [marginTop (pct 20)]
          `shouldEqual` "margin-top:20%"
      ]
    , it "should render generic arguments to the margin-top property correctly"
      [ renderProperties [marginTop initial]
          `shouldEqual` "margin-top:initial"
      , renderProperties [marginTop inherit]
          `shouldEqual` "margin-top:inherit"
      , renderProperties [marginTop auto]
          `shouldEqual` "margin-top:auto"
      , renderProperties [marginTop unset]
          `shouldEqual` "margin-top:unset"
      , renderProperties [marginTop (other "foo")]
          `shouldEqual` "margin-top:foo"
      , renderProperties [marginTop (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "margin-top:-webkit-foo;margin-top:-moz-foo"
      -- , renderProperties [marginTop all]
      --     `shouldEqual` "margin-top:should not compile"
      -- , renderProperties [marginTop baseline]
      --     `shouldEqual` "margin-top:should not compile"
      -- , renderProperties [marginTop center]
      --     `shouldEqual` "margin-top:should not compile"
      -- , renderProperties [marginTop none]
      --     `shouldEqual` "margin-top:should not compile"          
      -- , renderProperties [marginTop normal]
      --     `shouldEqual` "margin-top:should not compile"
      -- , renderProperties [marginTop visible]
      --     `shouldEqual` "margin-top:should not compile"
      -- , renderProperties [marginTop hidden]
      --     `shouldEqual` "margin-top:should not compile"
      ]
    , it "should render the margin-bottom property correctly"
      [ renderProperties [marginBottom (px 20)]
          `shouldEqual` "margin-bottom:20px"
      , renderProperties [marginBottom (pct 20)]
          `shouldEqual` "margin-bottom:20%"
      ]
    , it "should render generic arguments to the margin-bottom property correctly"
      [ renderProperties [marginBottom initial]
          `shouldEqual` "margin-bottom:initial"
      , renderProperties [marginBottom inherit]
          `shouldEqual` "margin-bottom:inherit"
      , renderProperties [marginBottom auto]
          `shouldEqual` "margin-bottom:auto"
      , renderProperties [marginBottom unset]
          `shouldEqual` "margin-bottom:unset"
      , renderProperties [marginBottom (other "foo")]
          `shouldEqual` "margin-bottom:foo"
      , renderProperties [marginBottom (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "margin-bottom:-webkit-foo;margin-bottom:-moz-foo"
      -- , renderProperties [marginBottom all]
      --     `shouldEqual` "margin-bottom:should not compile"
      -- , renderProperties [marginBottom baseline]
      --     `shouldEqual` "margin-bottom:should not compile"
      -- , renderProperties [marginBottom center]
      --     `shouldEqual` "margin-bottom:should not compile"
      -- , renderProperties [marginBottom none]
      --     `shouldEqual` "margin-bottom:should not compile"          
      -- , renderProperties [marginBottom normal]
      --     `shouldEqual` "margin-bottom:should not compile"
      -- , renderProperties [marginBottom visible]
      --     `shouldEqual` "margin-bottom:should not compile"
      -- , renderProperties [marginBottom hidden]
      --     `shouldEqual` "margin-bottom:should not compile"
      ]
    , it "should render the margin-left property correctly"
      [ renderProperties [marginLeft (px 20)]
          `shouldEqual` "margin-left:20px"
      , renderProperties [marginLeft (pct 20)]
          `shouldEqual` "margin-left:20%"
      ]
    , it "should render generic arguments to the margin-left property correctly"
      [ renderProperties [marginLeft initial]
          `shouldEqual` "margin-left:initial"
      , renderProperties [marginLeft inherit]
          `shouldEqual` "margin-left:inherit"
      , renderProperties [marginLeft auto]
          `shouldEqual` "margin-left:auto"
      , renderProperties [marginLeft unset]
          `shouldEqual` "margin-left:unset"
      , renderProperties [marginLeft (other "foo")]
          `shouldEqual` "margin-left:foo"
      , renderProperties [marginLeft (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "margin-left:-webkit-foo;margin-left:-moz-foo"
      -- , renderProperties [marginLeft all]
      --     `shouldEqual` "margin-left:should not compile"
      -- , renderProperties [marginLeft baseline]
      --     `shouldEqual` "margin-left:should not compile"
      -- , renderProperties [marginLeft center]
      --     `shouldEqual` "margin-left:should not compile"
      -- , renderProperties [marginLeft none]
      --     `shouldEqual` "margin-left:should not compile"          
      -- , renderProperties [marginLeft normal]
      --     `shouldEqual` "margin-left:should not compile"
      -- , renderProperties [marginLeft visible]
      --     `shouldEqual` "margin-left:should not compile"
      -- , renderProperties [marginLeft hidden]
      --     `shouldEqual` "margin-left:should not compile"
      ]      
    , it "should render the margin-right property correctly"
      [ renderProperties [marginRight (px 20)]
          `shouldEqual` "margin-right:20px"
      , renderProperties [marginRight (pct 20)]
          `shouldEqual` "margin-right:20%"
      ]
    , it "should render generic arguments to the margin-right property correctly"
      [ renderProperties [marginRight initial]
          `shouldEqual` "margin-right:initial"
      , renderProperties [marginRight inherit]
          `shouldEqual` "margin-right:inherit"
      , renderProperties [marginRight auto]
          `shouldEqual` "margin-right:auto"
      , renderProperties [marginRight unset]
          `shouldEqual` "margin-right:unset"
      , renderProperties [marginRight (other "foo")]
          `shouldEqual` "margin-right:foo"
      , renderProperties [marginRight (otherPrefixed [webkit_, moz_] "foo")]
          `shouldEqual` "margin-right:-webkit-foo;margin-right:-moz-foo"
      -- , renderProperties [marginRight all]
      --     `shouldEqual` "margin-right:should not compile"
      -- , renderProperties [marginRight baseline]
      --     `shouldEqual` "margin-right:should not compile"
      -- , renderProperties [marginRight center]
      --     `shouldEqual` "margin-right:should not compile"
      -- , renderProperties [marginRight none]
      --     `shouldEqual` "margin-right:should not compile"          
      -- , renderProperties [marginRight normal]
      --     `shouldEqual` "margin-right:should not compile"
      -- , renderProperties [marginRight visible]
      --     `shouldEqual` "margin-right:should not compile"
      -- , renderProperties [marginRight hidden]
      --     `shouldEqual` "margin-right:should not compile"
      ]
    ]
  ]
