module Css.PseudoTests where

import Spec exposing (..)
import Css.TestUtils exposing (it)

import Css exposing (renderProperties, renderCompact, (:|))
import Css.Box exposing (borderStyle)
import Css.Common exposing (..)
import Css.ColorsAndStrokes exposing (solid, disc)
import Css.Elements exposing (p)

import Css.Pseudo exposing (..)

suite : Spec
suite = describe "Css.PseudoTests"
  [ describe "The pseudo classes"
    [ it "should render correctly"
        [ let stylesheet =
            [ (p :| lastOfType) [ borderStyle solid ] [] ]
          in renderCompact stylesheet 
              `shouldEqual` "p:last-of-type {border-style:solid}"
        ]
    ]
  , describe "The pseudo functions"
    [ it "should render correctly"
        [ let stylesheet =
            [ (p :| lang "no") [ borderStyle solid ] [] ]
          in renderCompact stylesheet 
              `shouldEqual` "p:lang(no) {border-style:solid}"
        ]
    ]
  , describe "the content function"
    [ it "should render the various types of content properly"
      [ renderProperties [ content openQuote ]
          `shouldEqual` "content:open-quote"
      , renderProperties [ content (stringContent "prefix") ]
          `shouldEqual` "content:\"prefix\""
      , renderProperties [ content (attrContent "href") ]
          `shouldEqual` "content:attr(href)"
      , renderProperties [ content (urlContent "http://www.foo.com") ]
          `shouldEqual` "content:url(http://www.foo.com)"
      ]
    , it "should render counters properly"
      [ renderProperties [ content (counter "countername") ]
          `shouldEqual` "content:counter(countername)"
      , renderProperties [ content (styledCounter "countername" disc) ]
          `shouldEqual` "content:counter(countername,disc)"
      , renderProperties [ content (counters "countername" ".") ]
          `shouldEqual` "content:counters(countername,\".\")"
      , renderProperties [ content (styledCounters "countername" "." disc) ]
          `shouldEqual` "content:counters(countername,\".\",disc)"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ content initial ]
          `shouldEqual` "content:initial"
      , renderProperties [ content inherit ]
          `shouldEqual` "content:inherit"
      , renderProperties [ content none ]
          `shouldEqual` "content:none"      
      , renderProperties [ content normal ]
          `shouldEqual` "content:normal"
      , renderProperties [ content unset ]
          `shouldEqual` "content:unset"
      , renderProperties [ content (other "foo") ]
          `shouldEqual` "content:foo"
      -- Should not compile:
      -- , renderProperties [content all]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content auto]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content baseline]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content center]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content visible]
      --     `shouldEqual` "content:should not compile"
      -- , renderProperties [content hidden]
      --     `shouldEqual` "content:should not compile"
      ]
    ]
  , describe "the counter increment function"
    [ it "should render a single counter increment"
      [ renderProperties [ counterIncrement (counterId "fred") ]
          `shouldEqual` "counter-increment:fred"
      , renderProperties [ counterIncrement (withStep "fred" -2) ]
          `shouldEqual` "counter-increment:fred -2"
      ]
    , it "should render multiple counter increments"
      [ renderProperties 
          [ counterIncrements [ (counterId "fred"), (withStep "wilma" -2) ] ]
            `shouldEqual` "counter-increment:fred wilma -2"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ counterIncrement initial ]
          `shouldEqual` "counter-increment:initial"
      , renderProperties [ counterIncrement inherit ]
          `shouldEqual` "counter-increment:inherit"
      , renderProperties [ counterIncrement none ]
          `shouldEqual` "counter-increment:none"      
      , renderProperties [ counterIncrement unset ]
          `shouldEqual` "counter-increment:unset"
      , renderProperties [ counterIncrement (other "foo") ]
          `shouldEqual` "counter-increment:foo"
      -- Should not compile:
      -- , renderProperties [counterIncrement all]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement auto]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement baseline]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement center]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [ counterIncrement normal ]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement visible]
      --     `shouldEqual` "counter-increment:should not compile"
      -- , renderProperties [counterIncrement hidden]
      --     `shouldEqual` "counter-increment:should not compile"
      ]
    ]
  , describe "the counter reset functions"
    [ it "should render a single counter reset"
      [ renderProperties [ counterReset (counterId "fred") ]
          `shouldEqual` "counter-reset:fred"
      , renderProperties [ counterReset (resetTo "fred" -2) ]
          `shouldEqual` "counter-reset:fred -2"
      ]
    , it "should render multiple counter resets"
      [ renderProperties 
          [ counterResets [ (counterId "fred"), (resetTo "wilma" -2) ] ]
            `shouldEqual` "counter-reset:fred wilma -2"
      ]
    , it "should render generic properties properly"
      [ renderProperties [ counterReset initial ]
          `shouldEqual` "counter-reset:initial"
      , renderProperties [ counterReset inherit ]
          `shouldEqual` "counter-reset:inherit"
      , renderProperties [ counterReset none ]
          `shouldEqual` "counter-reset:none"      
      , renderProperties [ counterReset unset ]
          `shouldEqual` "counter-reset:unset"
      , renderProperties [ counterReset (other "foo") ]
          `shouldEqual` "counter-reset:foo"
      -- Should not compile:
      -- , renderProperties [counterReset all]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset auto]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset baseline]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset center]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [ counterReset normal ]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset visible]
      --     `shouldEqual` "counter-reset:should not compile"
      -- , renderProperties [counterReset hidden]
      --     `shouldEqual` "counter-reset:should not compile"
      ]
    ]
  ]
