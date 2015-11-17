module Css.FontTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.ColorsAndStrokes exposing (..)
import Css.Common exposing (..)
import Css.Font exposing (..)
import Css.Geometry exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.FontTests"
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
      [ renderProperties [ fontFamily ["Bingo"] [] ] 
          `shouldEqual` "font-family:\"Bingo\""
      , renderProperties [ fontFamily ["Bingo"] [sansSerif] ] 
          `shouldEqual` "font-family:\"Bingo\",sans-serif"
      , renderProperties [ fontFamily ["Bingo", "Weyerhauser"] [serif, fantasy] ] 
          `shouldEqual` "font-family:\"Bingo\",\"Weyerhauser\",serif,fantasy"
      ]
-- TODO      
    -- , it "can accept common properties initial, inherit, unset and other"
    --   [ renderProperties [fontColor initial] `shouldEqual` "color:initial"
    --   , renderProperties [fontColor inherit] `shouldEqual` "color:inherit" 
    --   , renderProperties [fontColor (other "wild-honey")] 
    --       `shouldEqual` "color:wild-honey" 
      -- Should not compile:
      -- , renderProperties [fontColor all]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor auto]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor baseline]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor center]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor normal]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor none]
      --     `shouldEqual` "color:should not compile"      
      -- , renderProperties [fontColor visible]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor hidden]
      --     `shouldEqual` "color:should not compile"
      -- , renderProperties [fontColor unset]
      --     `shouldEqual` "color:should not compile"          
      -- ]
    ]
  , describe "fontSize"
    [ it "should render named font sizes properly"
      [ renderProperties [ fontSize xxSmall ] 
          `shouldEqual` "font-size:xx-small"
      ]
--TODO      
    -- , it "should render numeric font sizes properly"
    --   [ renderProperties [ fontSize (px 20) ] 
    --       `shouldEqual` "font-size:20px"
    --   ]
    , it "can accept common properties initial, inherit, unset and other"
      [ renderProperties [fontSize initial] `shouldEqual` "font-size:initial"
      , renderProperties [fontSize inherit] `shouldEqual` "font-size:inherit" 
      , renderProperties [fontSize unset] `shouldEqual` "font-size:unset" 
      , renderProperties [fontSize (other "foo")] `shouldEqual` "font-size:foo" 
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
      -- , renderProperties [fontSize unset]
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
      -- , renderProperties [fontStyle unset]
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
      , renderProperties [fontVariant unset] `shouldEqual` "font-variant:unset" 
      , renderProperties [fontVariant (other "foo")] `shouldEqual` "font-variant:foo" 
      -- Should not compile:
      -- , renderProperties [fontVariant all]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant auto]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant baseline]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant center]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant none]
      --     `shouldEqual` "font-variant:should not compile"      
      -- , renderProperties [fontVariant visible]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant hidden]
      --     `shouldEqual` "font-variant:should not compile"
      -- , renderProperties [fontVariant unset]
      --     `shouldEqual` "font-variant:should not compile"          
      ]
    ]
  , describe "fontWeight"
    [ it "should render named font weights properly"
      [ renderProperties [ fontWeight bold ] 
          `shouldEqual` "font-weight:bold"
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
      -- , renderProperties [fontWeight unset]
      --     `shouldEqual` "font-weight:should not compile"          
      ]
    ]
  , describe "lineHeight"
    [ it "should render dimensioned line heights properly"
      [ renderProperties [ lineHeight (unitless 3.5) ] 
          `shouldEqual` "line-height:3.5"
      , renderProperties [lineHeight (px 20)] 
        `shouldEqual` "line-height:20px" 
      ]
-- TODO
{-
    , it "can accept common properties normal, initial, inherit, unset and other"
      [ renderProperties [lineHeight initial] `shouldEqual` "line-height:initial"
      , renderProperties [lineHeight inherit] `shouldEqual` "line-height:inherit" 
      , renderProperties [lineHeight normal] `shouldEqual` "line-height:normal" 
      , renderProperties [lineHeight unset] `shouldEqual` "line-height:unset" 
      , renderProperties [lineHeight (other "foo")] `shouldEqual` "line-height:foo" 
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
      -- , renderProperties [lineHeight unset]
      --     `shouldEqual` "line-height:should not compile"          
      ]
-}
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
                |> withVariant smallCaps
                |> withStyle italic)] 
            `shouldEqual` "font:italic small-caps bold 20px \"Hobo\""
      ]      
    , it "will accept common properties initial, inherit and other"
      [ renderProperties [font initial] `shouldEqual` "font:initial"
      , renderProperties [font inherit] `shouldEqual` "font:inherit" 
      , renderProperties [font unset] `shouldEqual` "font:unset"          
      , renderProperties [font (other "hbs")] `shouldEqual` "font:hbs" 
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
  ]
