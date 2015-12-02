module Css.BackgroundTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Background exposing (..)
import Css.ColorsAndStrokes exposing (..)
import Css.Common exposing (..)
import Css.Geometry exposing (..)
import Css.Layout exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.BackgroundTests"
  [ describe "background"
    -- TODO - Size combinator can actually take "auto" and image can take "none"
    [ it "should render combinations of the combinators properly"
      [ renderProperties 
          [ background (withPosition (placed sideLeft sideTop) Nothing) ]
          `shouldEqual` "background:left top"
      , renderProperties 
          [ background (withPosition 
                        (positioned (px 20) (pct 30)) 
                        (Just ((px 20) `by` (px 30)))) ]
          `shouldEqual` "background:20px 30%/20px 30px"
      , renderProperties 
          [ background (withBgColor green >> withRepeat roundRepeat) ]
          `shouldEqual` "background:round #73D216"
      , renderProperties 
          [ background (withImage (url "URL") >> 
                        withClip paddingBox >> 
                        withOrigin contentBox) ]
          `shouldEqual` "background:url(\"URL\") content-box padding-box"
      , renderProperties 
          [ background (withAttachment attachFixed) ]
          `shouldEqual` "background:fixed"
      ]  
    , it "should render generic properties properly" 
      [ renderProperties [ background initial ]
          `shouldEqual` "background:initial"
      , renderProperties [ background inherit ]
          `shouldEqual` "background:inherit"
      , renderProperties [ background unset ]
          `shouldEqual` "background:unset"          
      , renderProperties [ background (other "foo") ]
          `shouldEqual` "background:foo"
      , renderProperties [ background (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background:-webkit-foo;background:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ background all ]
      --     `shouldEqual` "background:should not compile"
      -- , renderProperties [ background auto ]
      --     `shouldEqual` "background:should not compile"
      -- , renderProperties [ background baseline ]
      --     `shouldEqual` "background:should not compile"
      -- , renderProperties [ background center ]
      --     `shouldEqual` "background:should not compile"
      -- , renderProperties [ background normal ]
      --     `shouldEqual` "background:should not compile"
      -- , renderProperties [ background none ]
      --     `shouldEqual` "background:should not compile"
      -- , renderProperties [ background visible ]
      --     `shouldEqual` "background:should not compile"
      -- , renderProperties [ background hidden ]
      --     `shouldEqual` "background:should not compile"
      ]
    ]  
  , describe "backgroundAttachment"
    [ it "should render properly"
      [ renderProperties [ backgroundAttachment attachFixed ]
          `shouldEqual` "background-attachment:fixed"
      , renderProperties [ backgroundAttachment attachScroll ]
          `shouldEqual` "background-attachment:scroll"
      , renderProperties [ backgroundAttachment attachLocal ]
          `shouldEqual` "background-attachment:local"
      ]  
    , it "should render generic attachment properties properly" 
      [ renderProperties [ backgroundAttachment initial ]
          `shouldEqual` "background-attachment:initial"
      , renderProperties [ backgroundAttachment inherit ]
          `shouldEqual` "background-attachment:inherit"
      , renderProperties [ backgroundAttachment unset ]
          `shouldEqual` "background-attachment:unset"          
      , renderProperties [ backgroundAttachment (other "foo") ]
          `shouldEqual` "background-attachment:foo"
      , renderProperties [ backgroundAttachment (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-attachment:-webkit-foo;background-attachment:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundAttachment all ]
      --     `shouldEqual` "background-attachment:should not compile"
      -- , renderProperties [ backgroundAttachment auto ]
      --     `shouldEqual` "background-attachment:should not compile"
      -- , renderProperties [ backgroundAttachment baseline ]
      --     `shouldEqual` "background-attachment:should not compile"
      -- , renderProperties [ backgroundAttachment center ]
      --     `shouldEqual` "background-attachment:should not compile"
      -- , renderProperties [ backgroundAttachment normal ]
      --     `shouldEqual` "background-attachment:should not compile"
      -- , renderProperties [ backgroundAttachment none ]
      --     `shouldEqual` "background-attachment:should not compile"
      -- , renderProperties [ backgroundAttachment visible ]
      --     `shouldEqual` "background-attachment:should not compile"
      -- , renderProperties [ backgroundAttachment hidden ]
      --     `shouldEqual` "background-attachment:should not compile"
      ]
    ]
  , describe "backgroundClip"
    [ it "should render properly"
      [ renderProperties [ backgroundClip paddingBox ]
          `shouldEqual` "background-clip:padding-box"
      , renderProperties [ backgroundClip borderBox ]
          `shouldEqual` "background-clip:border-box"
      , renderProperties [ backgroundClip contentBox ]
          `shouldEqual` "background-clip:content-box"
      ]  
    , it "should render generic clip properties properly" 
      [ renderProperties [ backgroundClip initial ]
          `shouldEqual` "background-clip:initial"
      , renderProperties [ backgroundClip inherit ]
          `shouldEqual` "background-clip:inherit"
      , renderProperties [ backgroundClip unset ]
          `shouldEqual` "background-clip:unset"          
      , renderProperties [ backgroundClip (other "foo") ]
          `shouldEqual` "background-clip:foo"
      , renderProperties [ backgroundClip (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-clip:-webkit-foo;background-clip:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundClip all ]
      --     `shouldEqual` "background-clip:should not compile"
      -- , renderProperties [ backgroundClip auto ]
      --     `shouldEqual` "background-clip:should not compile"
      -- , renderProperties [ backgroundClip baseline ]
      --     `shouldEqual` "background-clip:should not compile"
      -- , renderProperties [ backgroundClip center ]
      --     `shouldEqual` "background-clip:should not compile"
      -- , renderProperties [ backgroundClip normal ]
      --     `shouldEqual` "background-clip:should not compile"
      -- , renderProperties [ backgroundClip none ]
      --     `shouldEqual` "background-clip:should not compile"
      -- , renderProperties [ backgroundClip visible ]
      --     `shouldEqual` "background-clip:should not compile"
      -- , renderProperties [ backgroundClip hidden ]
      --     `shouldEqual` "background-clip:should not compile"
      ]
    ]
  , describe "backgroundColor"
    [ it "should render colors properly"
      [ renderProperties [ backgroundColor green ]
          `shouldEqual` "background-color:#73D216"
      , renderProperties [ backgroundColor (rgba 255 255 255 0.5) ]
          `shouldEqual` "background-color:rgba(255,255,255,0.5)"
      , renderProperties [ backgroundColor transparent ]
          `shouldEqual` "background-color:transparent"
      , renderProperties [ backgroundColor currentColor ]
          `shouldEqual` "background-color:currentColor"
      ]
    , it "should render generic colors properly" 
      [ renderProperties [ backgroundColor initial ]
          `shouldEqual` "background-color:initial"
      , renderProperties [ backgroundColor inherit ]
          `shouldEqual` "background-color:inherit"
      , renderProperties [ backgroundColor unset ]
          `shouldEqual` "background-color:unset"
      , renderProperties [ backgroundColor (other "foo") ]
          `shouldEqual` "background-color:foo"
      , renderProperties [ backgroundColor (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-color:-webkit-foo;background-color:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundColor all ]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [ backgroundColor auto ]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [ backgroundColor baseline ]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [ backgroundColor center ]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [ backgroundColor normal ]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [ backgroundColor none ]
      --     `shouldEqual` "background-color:should not compile"      
      -- , renderProperties [ backgroundColor visible ]
      --     `shouldEqual` "background-color:should not compile"
      -- , renderProperties [ backgroundColor hidden ]
      --     `shouldEqual` "background-color:should not compile"
      ]
    ]         
  , describe "backgroundImage"
    [ it "should render properly"
      [ renderProperties [ backgroundImage (url "http://some.image.com/") ]
          `shouldEqual` "background-image:url(\"http://some.image.com/\")"
      ]  
    , it "should render generic image properties properly" 
      [ renderProperties [ backgroundImage initial ]
          `shouldEqual` "background-image:initial"
      , renderProperties [ backgroundImage inherit ]
          `shouldEqual` "background-image:inherit"
      , renderProperties [ backgroundImage none ]
          `shouldEqual` "background-image:none"
      , renderProperties [ backgroundImage unset ]
          `shouldEqual` "background-image:unset"
      , renderProperties [ backgroundImage (other "foo") ]
          `shouldEqual` "background-image:foo"
      , renderProperties [ backgroundImage (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-image:-webkit-foo;background-image:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundImage all ]
      --     `shouldEqual` "background-image:should not compile"
      -- , renderProperties [ backgroundImage auto ]
      --     `shouldEqual` "background-image:should not compile"
      -- , renderProperties [ backgroundImage baseline ]
      --     `shouldEqual` "background-image:should not compile"
      -- , renderProperties [ backgroundImage center ]
      --     `shouldEqual` "background-image:should not compile"
      -- , renderProperties [ backgroundImage normal ]
      --     `shouldEqual` "background-image:should not compile"
      -- , renderProperties [ backgroundImage visible ]
      --     `shouldEqual` "background-image:should not compile"
      -- , renderProperties [ backgroundImage hidden ]
      --     `shouldEqual` "background-image:should not compile"
      ]
    ]
  , describe "backgroundOrigin"
    [ it "should render properly"
      [ renderProperties [ backgroundOrigin paddingBox ]
          `shouldEqual` "background-origin:padding-box"
      , renderProperties [ backgroundOrigin borderBox ]
          `shouldEqual` "background-origin:border-box"
      , renderProperties [ backgroundOrigin contentBox ]
          `shouldEqual` "background-origin:content-box"
      ]  
    , it "should render generic origin properties properly" 
      [ renderProperties [ backgroundOrigin initial ]
          `shouldEqual` "background-origin:initial"
      , renderProperties [ backgroundOrigin inherit ]
          `shouldEqual` "background-origin:inherit"
      , renderProperties [ backgroundOrigin unset ]
          `shouldEqual` "background-origin:unset"          
      , renderProperties [ backgroundOrigin (other "foo") ]
          `shouldEqual` "background-origin:foo"
      , renderProperties [ backgroundOrigin (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-origin:-webkit-foo;background-origin:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundOrigin all ]
      --     `shouldEqual` "background-origin:should not compile"
      -- , renderProperties [ backgroundOrigin auto ]
      --     `shouldEqual` "background-origin:should not compile"
      -- , renderProperties [ backgroundOrigin baseline ]
      --     `shouldEqual` "background-origin:should not compile"
      -- , renderProperties [ backgroundOrigin center ]
      --     `shouldEqual` "background-origin:should not compile"
      -- , renderProperties [ backgroundOrigin normal ]
      --     `shouldEqual` "background-origin:should not compile"
      -- , renderProperties [ backgroundOrigin none ]
      --     `shouldEqual` "background-origin:should not compile"
      -- , renderProperties [ backgroundOrigin visible ]
      --     `shouldEqual` "background-origin:should not compile"
      -- , renderProperties [ backgroundOrigin hidden ]
      --     `shouldEqual` "background-origin:should not compile"
      ]
    ]
  , describe "backgroundPosition"
    [ it "should render placed positions properly"
      [ renderProperties [ backgroundPosition (placed sideLeft sideTop) ]
          `shouldEqual` "background-position:left top"
      , renderProperties [ backgroundPosition (placed sideCenter sideMiddle) ]
          `shouldEqual` "background-position:center center"
      ]
    , it "should render positioned positions properly"
      [ renderProperties [ backgroundPosition (positioned (px 10) (px 20)) ]
          `shouldEqual` "background-position:10px 20px"
      , renderProperties [ backgroundPosition (positioned (pct 20) (pct 30)) ]
          `shouldEqual` "background-position:20% 30%"
      , renderProperties [ backgroundPosition (positioned (px 20) (pct 30)) ]
          `shouldEqual` "background-position:20px 30%"
      ]
    , it "should render generic positions properly"
      [ renderProperties [ backgroundPosition initial ]
          `shouldEqual` "background-position:initial"
      , renderProperties [ backgroundPosition inherit ]
          `shouldEqual` "background-position:inherit"
      , renderProperties [ backgroundPosition unset ]
          `shouldEqual` "background-position:unset"
      , renderProperties [ backgroundPosition (other "foo") ]
          `shouldEqual` "background-position:foo"
      , renderProperties [ backgroundPosition (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-position:-webkit-foo;background-position:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundPosition all ]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [ backgroundPosition auto ]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [ backgroundPosition baseline ]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [ backgroundPosition center ]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [ backgroundPosition normal ]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [ backgroundPosition none ]
      --     `shouldEqual` "background-position:should not compile"      
      -- , renderProperties [ backgroundPosition visible ]
      --     `shouldEqual` "background-position:should not compile"
      -- , renderProperties [ backgroundPosition hidden ]
      --     `shouldEqual` "background-position:should not compile"
      ]
    ]
  , describe "backgroundRepeat"
  -- TODO syntax that takes horizontal and vertical values separately
    [ it "should render properly"
      [ renderProperties [ backgroundRepeat repeat ]
          `shouldEqual` "background-repeat:repeat"
      , renderProperties [ backgroundRepeat space ]
          `shouldEqual` "background-repeat:space"
      ]  
    , it "should render generic repeat properties properly" 
      [ renderProperties [ backgroundRepeat initial ]
          `shouldEqual` "background-repeat:initial"
      , renderProperties [ backgroundRepeat inherit ]
          `shouldEqual` "background-repeat:inherit"
      , renderProperties [ backgroundRepeat unset ]
          `shouldEqual` "background-repeat:unset"
      , renderProperties [ backgroundRepeat (other "foo") ]
          `shouldEqual` "background-repeat:foo"
      , renderProperties [ backgroundRepeat (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-repeat:-webkit-foo;background-repeat:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundRepeat all ]
      --     `shouldEqual` "background-repeat:should not compile"
      -- , renderProperties [ backgroundRepeat auto ]
      --     `shouldEqual` "background-repeat:should not compile"
      -- , renderProperties [ backgroundRepeat baseline ]
      --     `shouldEqual` "background-repeat:should not compile"
      -- , renderProperties [ backgroundRepeat center ]
      --     `shouldEqual` "background-repeat:should not compile"
      -- , renderProperties [ backgroundRepeat normal ]
      --     `shouldEqual` "background-repeat:should not compile"
      -- , renderProperties [ backgroundRepeat none ]
      --     `shouldEqual` "background-repeat:should not compile"
      -- , renderProperties [ backgroundRepeat visible ]
      --     `shouldEqual` "background-repeat:should not compile"
      -- , renderProperties [ backgroundRepeat hidden ]
      --     `shouldEqual` "background-repeat:should not compile"
      ]
    ]
  , describe "backgroundSize"
    [ it "should render named sizes properly"
      [ renderProperties [ backgroundSize cover ]
          `shouldEqual` "background-size:cover"
      , renderProperties [ backgroundSize contain ]
          `shouldEqual` "background-size:contain"
      ]
      -- TODO two-value syntax can take "auto" twice or in the first position.
    , it "should render dimensioned sizes properly"
      [ renderProperties [ backgroundSize ((px 20) `by` (px 30)) ]
          `shouldEqual` "background-size:20px 30px"
      , renderProperties [ backgroundSize (20 |> px |> bgWidth) ]
          `shouldEqual` "background-size:20px auto"
      ]
    , it "should render generic sizes properly" 
      [ renderProperties [ backgroundSize initial ]
          `shouldEqual` "background-size:initial"
      , renderProperties [ backgroundSize inherit ]
          `shouldEqual` "background-size:inherit"
      , renderProperties [ backgroundSize unset ]
          `shouldEqual` "background-size:unset"
      , renderProperties [ backgroundSize auto ]
          `shouldEqual` "background-size:auto"
      , renderProperties [ backgroundSize (other "foo") ]
          `shouldEqual` "background-size:foo"
      , renderProperties [ backgroundSize (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "background-size:-webkit-foo;background-size:-moz-foo"          
      -- Should not compile:
      -- , renderProperties [ backgroundSize all ]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [ backgroundSize baseline ]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [ backgroundSize center ]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [ backgroundSize normal ]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [ backgroundSize none ]
      --     `shouldEqual` "background-size:should not compile"      
      -- , renderProperties [ backgroundSize visible ]
      --     `shouldEqual` "background-size:should not compile"
      -- , renderProperties [ backgroundSize hidden ]
      --     `shouldEqual` "background-size:should not compile"
      ]    
    ]
  ]
