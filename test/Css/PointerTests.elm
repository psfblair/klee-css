module Css.PointerTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Common exposing (..)
import Css.Pointer exposing (..)
import Css exposing (renderProperties)

suite : Spec
suite = describe "Css.PointerTests"
  [ describe "The cursor function"
    [ it "should render specific cursors properly"
      [ renderProperties [ cursor aliasCursor ] 
          `shouldEqual` "cursor:alias"
      , renderProperties [ cursor (cursorUrl "http://www.foo.com") ] 
          `shouldEqual` "cursor:url(\"http://www.foo.com\")"
      ]

    , it "should render the generic properties properly"
      [ renderProperties [ cursor initial ] `shouldEqual` "cursor:initial"
      , renderProperties [ cursor inherit ] `shouldEqual` "cursor:inherit"
      , renderProperties [ cursor auto    ] `shouldEqual` "cursor:auto"
      , renderProperties [ cursor none    ] `shouldEqual` "cursor:none"
      , renderProperties [ cursor unset   ] `shouldEqual` "cursor:unset"
      , renderProperties [ cursor (other "foo") ] 
          `shouldEqual` "cursor:foo"
      , renderProperties [ cursor (otherPrefixed [webkit_, moz_] "foo") ] 
          `shouldEqual` "cursor:-webkit-foo;cursor:-moz-foo"
      -- , renderProperties [ cursor all ]
      --     `shouldEqual` "cursor:should not compile"
      -- , renderProperties [ cursor baseline ]
      --     `shouldEqual` "cursor:should not compile"
      -- , renderProperties [ cursor center ]
      --     `shouldEqual` "cursor:should not compile"
      -- , renderProperties [ cursor normal ]
      --     `shouldEqual` "cursor:should not compile"
      -- , renderProperties [ cursor visible ]
      --     `shouldEqual` "cursor:should not compile"
      -- , renderProperties [ cursor hidden ]
      --     `shouldEqual` "cursor:should not compile"
      ]
    ]
    , describe "The pointerEvents function"
      [ it "should render specific pointer events properly"
        [ renderProperties [ pointerEvents visiblePainted ] 
            `shouldEqual` "pointer-events:visiblePainted"
        ]
      , it "should render the generic properties properly"
        [ renderProperties [ pointerEvents initial] 
            `shouldEqual` "pointer-events:initial"
        , renderProperties [ pointerEvents inherit] 
            `shouldEqual` "pointer-events:inherit"
        , renderProperties [ pointerEvents auto]
            `shouldEqual` "pointer-events:auto"
        , renderProperties [ pointerEvents none]
            `shouldEqual` "pointer-events:none"
        , renderProperties [ pointerEvents unset] 
            `shouldEqual` "pointer-events:unset"
        , renderProperties [ pointerEvents (other "foo")] 
            `shouldEqual` "pointer-events:foo"
        , renderProperties [ pointerEvents (otherPrefixed [webkit_, moz_] "foo")] 
            `shouldEqual` "pointer-events:-webkit-foo;pointer-events:-moz-foo"
        -- , renderProperties [ pointerEvents all]
        --     `shouldEqual` "pointer-events:should not compile"
        -- , renderProperties [ pointerEvents baseline]
        --     `shouldEqual` "pointer-events:should not compile"
        -- , renderProperties [ pointerEvents center]
        --     `shouldEqual` "pointer-events:should not compile"
        -- , renderProperties [ pointerEvents normal]
        --     `shouldEqual` "pointer-events:should not compile"
        -- , renderProperties [ pointerEvents visible]
        --     `shouldEqual` "pointer-events:should not compile"
        -- , renderProperties [ pointerEvents hidden]
        --     `shouldEqual` "pointer-events:should not compile"
        ]
      ]
    ]
    
