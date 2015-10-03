module Css.Stylesheet where

import Css.Common
import Css.Property exposing (Value)
import Css.Selector exposing (Selector, Refinement)

{-| Types for the @media rule: @media not|only mediatype and (media feature) { rules }
    The MediaQuery type does not contain the media rules themselves.
-}
type MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType [Feature]
type MediaType = MediaType Value
type NotOrOnly = Not | Only
type Feature = Feature String (Maybe Value)

{-| A @keyframes rule: @keyframes animation-name {keyframes-selector {css-styles;}}
    where keyframes-selector is the percentage of the animation duration.
-}
type Keyframes = Keyframes String [(Float, [Rule])]

type Scope
  = Self   Refinement
  | Root   Selector
  | Pop    Integer
  | Child  Selector
  | Sub    Selector

type Rule
  = Property (Key ()) Value
  | Nested   Scope [Rule]
  | Query    MediaQuery [Rule]
  | Face     [Rule]
  | Keyframe Keyframes
  | Import   String

{-| In Clay, the `Css` context is used to collect style rules which are mappings
from selectors to style properties. The `Css` type is a computation in the
`StyleM` monad that just collects and doesn't return anything. -}

type Css = Css [Rule]

runS : Css -> [Rule]
runS (Css rules) = rules

addRule : Css -> Rule -> Css
addRule (Css rules) ruleToAdd = rules ++ [ ruleToAdd ] |> Css

{- In Clay, the key function adds a new style property to the stylesheet with
the specified `Key` and value. The value can be any type that is in the `Val'
typeclass; i.e., that can be converted to a `Value`. No typeclasses in the Elm version! -}

-- key : Css -> Key a -> a -> Css
-- key css k v = Property (cast k) (value v) |> addRule css

{- In Clay, the prefixed function adds a new style property to the stylesheet
with the specified `Key` and value in the same way `key` does, but uses a
`PrefixedOrNot` key. Again, not available in Elm. -}

-- prefixed : Css -> PrefixedOrNot -> a -> Css
-- prefixed css prefixedOrNot = key css (Key prefixedOrNot)

{-| The fallback operator can be used to add style rules to the current context
for which there is no typed version available. Both the key and the value
are plain text values and rendered as is to the output CSS. -}

infixl 4 -:
(-:) : Css -> Key String -> String -> Css
(-:) css k v = Property (cast k) (stringValueWrapper.value v) |> addRule css

{-| Assign a group of style rules to a selector. When the selector is nested inside an
outer scope it will be composed with `deep`, which maps to @sel1 sel2@ in CSS.  -}

infixr 5 ?
(?) : Selector -> Css -> Css
(?) sel css = Nested (Sub sel) (runS css) |> addRule

{-| Assign a group of style rules to a selector. When the selector is nested inside
an outer scope it will be composed with `child`, which maps to @sel1 > sel2@ in CSS. -}

infixr 5 <?
(<?) : Selector -> Css -> Css
(<?) sel css = Nested (Child sel) (runS css) |> addRule

{-| Assign a group of style rules to a filter selector. When the selector is nested
inside an outer scope it will be composed with the `with` selector, which maps
to something like @sel#filter@ or @sel.filter@ in CSS depending on the filter. -}

infixr 5 &
(&) : Refinement -> Css -> Css
(&) refinemt css = Nested (Self refinemt) (runS css) |> addRule

{-| `root` is used to add style rules to the top scope. -}

root : Selector -> Css -> Css
root sel css = Nested (Root sel) (runS css) |> addRule

{-| `pop` is used to add style rules to selectors defined in an outer scope. The
counter specifies how far up the scope stack we want to add the rules. -}

pop : Int -> Css -> Css
pop numLevels css = Nested (Pop numLevels) (runS css) |> addRule

-------------------------------------------------------------------------------

{-| A @media rule to apply a set of style rules when a media type and
feature queries apply. -}

query : MediaType -> [Feature] -> Css -> Css
query typeOfMedia mediaFeatures mediaRules =
  runS mediaRules
  |> Query (MediaQuery Nothing typeOfMedia mediaFeatures)
  |> addRule

{-| A  @media rule to apply a set of style rules when the media type and
feature queries do not apply. -}

queryNot : MediaType -> [Feature] -> Css -> Css
queryNot typeOfMedia mediaFeatures mediaRules =
  runS mediaRules
  |> Query (MediaQuery (Just Not) typeOfMedia mediaFeatures)
  |> addRule

{-| A  @media rule to apply a set of style rules only when the media type and
feature queries apply. -}

queryOnly : MediaType -> [Feature] -> Css -> Css
queryOnly typeOfMedia mediaFeatures mediaRules =
  runS mediaRules
  |> Query (MediaQuery (Just Only) typeOfMedia mediaFeatures)
  |> addRule

-------------------------------------------------------------------------------
{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs. -}

keyframes : String -> [(Float, Css)] -> Css
keyframes animationName frames =
  frames
  |> map \(percentage, css) -> (percentage, runS css)
  |> Keyframes animationName
  |> Keyframe
  |> addRule

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules. -}

keyframesFromTo : String -> Css -> Css -> Css
keyframesFromTo animationName fromRules toRules =
  keyframes animationName [(0, fromRules), (100, toRules)]

-------------------------------------------------------------------------------

{-| Create a CSS @font-face rule from some css specifying font properties. -}

fontFace : Css -> Css
fontFace fontProperties = runS fontProperties |> Face |> addRule

-------------------------------------------------------------------------------

{-| Create a CSS @import rule to import a CSS file from a URL -}

importUrl : String -> Css
importUrl url = Import url |> addRule
