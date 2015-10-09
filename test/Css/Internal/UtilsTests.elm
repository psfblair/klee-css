module Css.Internal.UtilsTests where

import Spec exposing (..)

import Css.Internal.Utils exposing (..)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.UtilsTests"
  [ -- floatModTest, toFixedTest, toHexStringTest,
  fromHexTest, fromHexCharTest ]

fromHexTest : Spec
fromHexTest =
  describe "fromHex"
    [ fromHex "#FF" `shouldEqual` (Ok  255)
    , fromHex "00" `shouldEqual`  (Ok    0)
    , fromHex "#10" `shouldEqual` (Ok   16)
    , fromHex "#0A" `shouldEqual` (Ok   10)
    , fromHex "FFF" `shouldEqual` (Ok 4095)
    ]

fromHexCharTest : Spec
fromHexCharTest =
  describe "fromHexChar"
    [ fromHexChar '0' `shouldEqual` (Ok  0)
    , fromHexChar '1' `shouldEqual` (Ok  1)
    , fromHexChar '2' `shouldEqual` (Ok  2)
    , fromHexChar '3' `shouldEqual` (Ok  3)
    , fromHexChar '4' `shouldEqual` (Ok  4)
    , fromHexChar '5' `shouldEqual` (Ok  5)
    , fromHexChar '6' `shouldEqual` (Ok  6)
    , fromHexChar '7' `shouldEqual` (Ok  7)
    , fromHexChar '8' `shouldEqual` (Ok  8)
    , fromHexChar '9' `shouldEqual` (Ok  9)
    , fromHexChar 'a' `shouldEqual` (Ok 10)
    , fromHexChar 'A' `shouldEqual` (Ok 10)
    , fromHexChar 'b' `shouldEqual` (Ok 11)
    , fromHexChar 'B' `shouldEqual` (Ok 11)
    , fromHexChar 'c' `shouldEqual` (Ok 12)
    , fromHexChar 'C' `shouldEqual` (Ok 12)
    , fromHexChar 'd' `shouldEqual` (Ok 13)
    , fromHexChar 'D' `shouldEqual` (Ok 13)
    , fromHexChar 'e' `shouldEqual` (Ok 14)
    , fromHexChar 'E' `shouldEqual` (Ok 14)
    , fromHexChar 'f' `shouldEqual` (Ok 15)
    , fromHexChar 'F' `shouldEqual` (Ok 15)
    , fromHexChar 'G' `shouldEqual` (Err "could not convert char 'G' to Int")
    ]
