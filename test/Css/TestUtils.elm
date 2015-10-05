module Css.TestUtils where

import Spec exposing (Spec, describe)

it : String -> List Spec -> Spec
it = describe
