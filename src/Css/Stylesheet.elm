module Css.Stylesheet where

import Css.Common
import Css.Property exposing (Value)
import Css.Selector exposing (Selector, Refinement)

type MediaType = MediaType Value
type NotOrOnly = Not | Only
type Feature = Feature String (Maybe Value)
type MediaQuery = MediaQuery (Maybe NotOrOnly) MediaType [Feature]

type App
  = Self   Refinement
  | Root   Selector
  | Pop    Integer
  | Child  Selector
  | Sub    Selector
    
type Rule
  = Property (Key ()) Value
  | Nested   App [Rule]
  | Query    MediaQuery [Rule]
  | Face     [Rule]
  | Keyframe Keyframes
  | Import   String

{-| A @keyframes rule: @keyframes animation-name {keyframes-selector {css-styles;}}
    where keyframes-selector is the percentage of the animation duration.
-}
type Keyframes = Keyframes String [(Float, [Rule])]

type StyleM a = S (Writer [Rule] a)
                -- deriving (Functor, Applicative, Monad)

runS : Css -> [Rule]
runS (S a) = execWriter a

rule : Rule -> Css
rule a = S (tell [a])
                                                                   
-- The `Css` context is used to collect style rules which are mappings from
-- selectors to style properties. The `Css` type is a computation in the
-- `StyleM` monad that just collects and doesn't return anything.

type Css = StyleM ()

-- instance Monoid Css where
-- mempty = pure ()
-- mappend = liftA2 mappend
                            

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value. The value can be any type that is in the `Val' typeclass, with other
-- words: can be converted to a `Value`.

key : Val a => Key a -> a -> Css
key k v = Property (cast k) (value v) |> rule

-- | Add a new style property to the stylesheet with the specified `Key` and
-- value, like `key` but use a `Prefixed` key.

prefixed : Val a => Prefixed -> a -> Css
prefixed xs = key (Key xs)
                                                         
-- | The colon operator can be used to add style rules to the current context
-- for which there is no embedded version available. Both the key and the value
-- are plain text values and rendered as is to the output CSS.
-- infix4 =:
(-:) : Key String -> String -> Css
(-:) = key

-- | Assign a stylesheet to a selector. When the selector is nested inside an
-- outer scope it will be composed with `deep`.
-- infixr 5 ?
(?) : Selector -> Css -> Css
(?) sel rs = rule $ Nested (Sub sel) (runS rs)
             
-- | Assign a stylesheet to a selector. When the selector is nested inside an
-- outer scope it will be composed with `|>`.
-- infixr 5 <?
(<?) : Selector -> Css -> Css
(<?) sel rs = Nested (Child sel) (runS rs) |> rule
                                               
-- | Assign a stylesheet to a filter selector. When the selector is nested
-- inside an outer scope it will be composed with the `with` selector.
-- infixr 5 &

(&) : Refinement -> Css -> Css
(&) p rs = Nested (Self p) (runS rs) |> rule

-- | Root is used to add style rules to the top scope.

root : Selector -> Css -> Css
root sel rs = Nested (Root sel) (runS rs) |> rule

-- | Pop is used to add style rules to selectors defined in an outer scope. The
-- counter specifies how far up the scope stack we want to add the rules.

pop : Int -> Css -> Css
pop i rs = Nested (Pop i) (runS rs) |> rule

-------------------------------------------------------------------------------

{-| A media query to apply a set of style rules when a media type and
feature queries apply. -}

query : MediaType -> [Feature] -> Css -> Css
query typeOfMedia mediaFeatures mediaRules
  = runS mediaRules
  |> Query (MediaQuery Nothing typeOfMedia mediaFeatures)
  |> rule

{-| A media query to apply a set of style rules when the media type and
feature queries do not apply. -}

queryNot : MediaType -> [Feature] -> Css -> Css
queryNot typeOfMedia mediaFeatures mediaRules
  = runS mediaRules
  |> Query (MediaQuery (Just Not) typeOfMedia mediaFeatures)
  |> rule

{-| A media query to apply a set of style rules only when the media type and
feature queries apply. -}

queryOnly : MediaType -> [Feature] -> Css -> Css
queryOnly typeOfMedia mediaFeatures mediaRules
  = runS mediaRules
  |> Query (MediaQuery (Just Only) typeOfMedia mediaFeatures)
  |> rule

-------------------------------------------------------------------------------
{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs. -}

keyframes : String -> [(Float, Css)] -> Css
keyframes animationName frames
  = frames
  |> map \(percentage, css) -> (percentage, runS css)
  |> Keyframes animationName
  |> Keyframe
  |> rule

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules. -}

keyframesFromTo : String -> Css -> Css -> Css
keyframesFromTo animationName fromRules toRules =
  keyframes animationName [(0, fromRules), (100, toRules)]
 
-------------------------------------------------------------------------------

{-| Create a CSS @font-face rule from some css specifying font properties. -}

fontFace : Css -> Css
fontFace fontProperties = fontProperties |> runS |> Face |> rule

{-| Import a CSS file from a URL -}

importUrl : String -> Css
importUrl url = Import url |> rule
