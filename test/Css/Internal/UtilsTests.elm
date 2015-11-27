module Css.Internal.UtilsTests where

import Spec exposing (..)

import Css.Internal.Utils exposing (..)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.UtilsTests"
  [ -- floatModTest, toFixedTest, 
    describe  "toHexStringTest"
    [ toHexString 1 0 `shouldEqual` "0"
    , toHexString 2 0 `shouldEqual` "00"
    , toHexString 1 15 `shouldEqual` "F"
    , toHexString 2 15 `shouldEqual` "0F"
    , toHexString 1 16 `shouldEqual` "10"
    , toHexString 2 255 `shouldEqual` "FF"
    , toHexString 3 240 `shouldEqual` "0F0"
    , toHexString 2 128 `shouldEqual` "80"
    , toHexString 2 256 `shouldEqual` "100"
    , toHexString 4 4095 `shouldEqual` "0FFF"
    ]
  , describe "fromHex"
    [ fromHex "#FF" `shouldEqual` (Ok  255)
    , fromHex "0" `shouldEqual`   (Ok    0)
    , fromHex "00" `shouldEqual`  (Ok    0)
    , fromHex "1" `shouldEqual`   (Ok    1)
    , fromHex "2" `shouldEqual`   (Ok    2)
    , fromHex "3" `shouldEqual`   (Ok    3)
    , fromHex "4" `shouldEqual`   (Ok    4)
    , fromHex "5" `shouldEqual`   (Ok    5)
    , fromHex "6" `shouldEqual`   (Ok    6)
    , fromHex "7" `shouldEqual`   (Ok    7)
    , fromHex "8" `shouldEqual`   (Ok    8)
    , fromHex "9" `shouldEqual`   (Ok    9)
    , fromHex "a" `shouldEqual`   (Ok   10)
    , fromHex "A" `shouldEqual`   (Ok   10)
    , fromHex "b" `shouldEqual`   (Ok   11)
    , fromHex "B" `shouldEqual`   (Ok   11)
    , fromHex "c" `shouldEqual`   (Ok   12)
    , fromHex "C" `shouldEqual`   (Ok   12)
    , fromHex "d" `shouldEqual`   (Ok   13)
    , fromHex "D" `shouldEqual`   (Ok   13)
    , fromHex "e" `shouldEqual`   (Ok   14)
    , fromHex "E" `shouldEqual`   (Ok   14)
    , fromHex "f" `shouldEqual`   (Ok   15)
    , fromHex "F" `shouldEqual`   (Ok   15)
    , fromHex "G" `shouldEqual`   (Err "could not convert char 'G' to Int")    
    , fromHex "#10" `shouldEqual` (Ok   16)
    , fromHex "#0A" `shouldEqual` (Ok   10)
    , fromHex "FFF" `shouldEqual` (Ok 4095)
    ]
  , describe "mapPairwise"
    [ let list1 = ["*.class1", "p.class2"]
          list2 = ["a.class3", "li.class5"]
          childJoin a b = a ++ " > " ++ b
      in (mapPairwise childJoin list1 list2) `shouldEqual`
          ["*.class1 > a.class3",
           "*.class1 > li.class5",
           "p.class2 > a.class3",
           "p.class2 > li.class5"]
    ]
  ]
