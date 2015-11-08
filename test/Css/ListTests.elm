module Css.ListTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css.Common exposing (..)
import Css.List exposing (..)
import Css exposing (renderProperties)

-------------------------------------------------------------------------------

suite : Spec
suite = describe "Css.ListTests"
  [ describe "The list style type function"
    [ it "should render a named style type properly"
      [ renderProperties [listStyleType disc]
          `shouldEqual` "list-style-type:disc"
      ]
    , it "should render generic style types properly"
      [ renderProperties [listStyleType none]
          `shouldEqual` "list-style-type:none"
      , renderProperties [listStyleType inherit]
          `shouldEqual` "list-style-type:inherit"
      , renderProperties [listStyleType initial]
          `shouldEqual` "list-style-type:initial"
      , renderProperties [listStyleType (other "foo")]
          `shouldEqual` "list-style-type:foo"
      -- Should not compile:
      -- , renderProperties [listStyleType all]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType auto]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType baseline]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType center]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType normal]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType visible]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType hidden]
      --     `shouldEqual` "list-style-type:should not compile"
      -- , renderProperties [listStyleType unset]
      --     `shouldEqual` "list-style-type:should not compile"
      ]  
    ]
  , describe "The list style position function"
    [ it "should render a named style position properly"
      [ renderProperties [listStylePosition inside]
          `shouldEqual` "list-style-position:inside"
      ]
    , it "should render generic style positions properly"
      [  renderProperties [listStylePosition inherit]
          `shouldEqual` "list-style-position:inherit"
      , renderProperties [listStylePosition initial]
          `shouldEqual` "list-style-position:initial"
      , renderProperties [listStylePosition (other "foo")]
          `shouldEqual` "list-style-position:foo"
      -- Should not compile:
      -- , renderProperties [listStylePosition all]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition auto]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition baseline]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition center]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition none]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition normal]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition visible]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition hidden]
      --     `shouldEqual` "list-style-position:should not compile"
      -- , renderProperties [listStylePosition unset]
      --     `shouldEqual` "list-style-position:should not compile"
      ]  
    ]
  , describe "The list style image function"
    [ it "should render a named style image properly"
      [ renderProperties [listStyleImage (imageUrl "http://www.foo.com")]
          `shouldEqual` "list-style-image:url(\"http://www.foo.com\")"
      ]
    , it "should render generic style images properly"
      [  renderProperties [listStyleImage inherit]
          `shouldEqual` "list-style-image:inherit"
      , renderProperties [listStyleImage initial]
          `shouldEqual` "list-style-image:initial"
      , renderProperties [listStyleImage none]
          `shouldEqual` "list-style-image:none"
      , renderProperties [listStyleImage (other "foo")]
          `shouldEqual` "list-style-image:foo"
      -- Should not compile:
      -- , renderProperties [listStyleImage all]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage auto]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage baseline]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage center]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage normal]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage visible]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage hidden]
      --     `shouldEqual` "list-style-image:should not compile"
      -- , renderProperties [listStyleImage unset]
      --     `shouldEqual` "list-style-image:should not compile"
      ]  
    ]  
  , describe "The list style shorthand function"
    [ it "should render combinations of styles properly"
      [ renderProperties 
          [ listStyle (withListImage (imageUrl "http://www.foo.com")) ]
            `shouldEqual` "list-style:url(\"http://www.foo.com\")"
      , renderProperties 
          [ listStyle (withListType disc >> withListPosition inside) ]
            `shouldEqual` "list-style:disc inside"
      ]
    , it "should render generic styles properly"
      [  renderProperties [ listStyle inherit ]
          `shouldEqual` "list-style:inherit"
      , renderProperties [ listStyle initial ]
          `shouldEqual` "list-style:initial"
      , renderProperties [ listStyle (other "foo") ]
          `shouldEqual` "list-style:foo"
      -- Should not compile:
      -- , renderProperties [listStyle all]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle auto]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle baseline]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle center]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle none]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle normal]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle visible]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle hidden]
      --     `shouldEqual` "list-style:should not compile"
      -- , renderProperties [listStyle unset]
      --     `shouldEqual` "list-style:should not compile"
      ]  
    ]  
  ]
