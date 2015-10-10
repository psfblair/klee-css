module Css
  ( Css, CssGenerator, Selector, Refinement, custom, (<?), (&), root
  , (.*), (.>), (.+), (.|), (.|#), (.|.), pseudo, func, withAttr
  , (.|@), (.|^), (.|$), (.|*), (.|~), (.|-)
  , MediaType, Feature, query, queryNot, queryOnly
  , keyframes, keyframesFromTo, fontFace, importUrl
  , Scope, Config, render, renderCompact, renderWith
  )  where

{-| A module for constructing Css in a typesafe way, and rendering the result
to a string (either pretty-printed or compact). This library is based on the
Haskell Clay CSS preprocessing framework, though some of the operators in the
selector DSL have been changed in order to avoid clashing with standard Elm
operators and for various other reasons.

The functions in this main module should be used in preference to those in
Css.Stylesheet, Css.Selector, Css.Render, which they export. Besides these,
the module Css.Property is also internal.

# Principal Types
@docs Css, CssGenerator, Selector, Refinement

# Operators for aggregating style rules
@docs custom, (<?), (&), root

# The selector language.
For predefined element selectors, see `Css.Elements`.
@docs (.*), (.>), (.+), (.|), (.|#), (.|.), pseudo, func, withAttr,
      (.|@), (.|^), (.|$), (.|*), (.|~), (.|-)

# Rendering stylesheets to CSS strings
@docs Scope, Config, render, renderCompact, renderWith

# Special rules
@docs MediaType, Feature, query, queryNot, queryOnly, keyframes, keyframesFromTo,
      fontFace, importUrl
-}

import Css.Internal.Stylesheet exposing
  ( Css, CssGenerator, SelectorScope
  , custom, addStylesAsChild, addFilteredStyles, root
  , MediaType, Feature, query, queryNot, queryOnly
  , keyframes, keyframesFromTo, fontFace, importUrl
  )

import Css.Internal.Selector exposing
  ( Selector, Refinement, star, deep, child, adjacent, with, byId, byClass
  , pseudo, func, withAttr, withAttrValue, withAttrValueBeginning, withAttrValueEnding
  , withAttrValueEnding, withAttrValueContaining, withAttrValueInSpacedList
  , withAttrValueInHyphenatedList
  )

import Css.Internal.Render exposing (renderWith, pretty, compact, Config)

-------------------------------------------------------------------------------
-- * Principal types

{-| The `Css` type is used to collect style rules which are mappings
from selectors to style properties.
-}
type alias Css = Css.Internal.Stylesheet.Css

{-| An alias for a function of Css -> Css, which is the type allowing for the
css combinators to be composed.
-}
type alias CssGenerator a = Css.Internal.Stylesheet.CssGenerator a

{-|  A type to represent the selector in a CSS rule.
-}
type alias Selector = Css.Internal.Selector.Selector

{-|  A type to represent refinements on a selector; i.e., classes, ids, attributes
etc. that may be applied to a selector at a given level as filters (but not child
selectors).
-}
type alias Refinement = Css.Internal.Selector.Refinement

-------------------------------------------------------------------------------
-- * Operators for aggregating style rules.

{-| The custom function can be used to add style rules to the current context
for which there is no typed version available. Both the key and the value
are plain text values and rendered as is to the output CSS.
-}
custom : String -> String -> Css -> Css
custom = Css.Internal.Stylesheet.custom
infixl 4 -:

{-| Assign a group of style rules to a selector. When the selector is nested inside
an outer scope it will be composed with `child`, which maps to @sel1 > sel2@ in CSS.
-}
(<?) : (List (CssGenerator a) -> CssGenerator Selector) -> List (CssGenerator a) -> CssGenerator Selector
(<?) = Css.Internal.Stylesheet.addStylesAsChild
infixr 5 <?

{-| Assign a group of style rules to a filter selector. When the selector is nested
inside an outer scope it will be composed with the `with` selector, which maps
to something like @sel#filter@ or @sel.filter@ in CSS depending on the filter.
-}
(&) : Refinement -> List (CssGenerator a) -> CssGenerator ()
(&) = Css.Internal.Stylesheet.addFilteredStyles
infixr 5 &

{-| `root` is used to add style rules to the top scope.
-}
root : Selector -> List (CssGenerator a) -> CssGenerator ()
root = Css.Internal.Stylesheet.root

-------------------------------------------------------------------------------
-- * The selector language.
-- ** Composing and refining selectors

{-| The deep selector composer. Maps to @sel1 sel2@ in CSS.
-}
(.*) : Selector -> Selector -> Selector
(.*) = Css.Internal.Selector.deep

{-| The child selector composer. Maps to @sel1 > sel2@ in CSS.
-}
(.>) : Selector -> Selector -> Selector
(.>) = Css.Internal.Selector.child

{-| The adjacent selector composer. Maps to @sel1 + sel2@ in CSS.
-}
(.+) : Selector -> Selector -> Selector
(.+) = Css.Internal.Selector.adjacent

{-| The filter selector composer, which adds a filter to a selector. Maps to
something like @sel#filter@ or @sel.filter@ in CSS, depending on the filter.
-}
(.|) : Selector -> Refinement -> Selector
(.|) = Css.Internal.Selector.with

{-| Given an id and a selector, add an id filter to the selector.
-}
(.|#) : String -> Selector -> Selector
(.|#) = Css.Internal.Selector.byId

{-| Given a class name and a selector, add a class filter to the selector.
-}
(.|.) : String -> Selector -> Selector
(.|.) = Css.Internal.Selector.byClass

{-| Filter elements of the given selector by pseudo selector or pseudo class.
The preferred syntax is to use one of the predefined pseudo selectors from "Css.Pseudo".
-}
pseudo : String -> Selector -> Selector
pseudo = Css.Internal.Selector.pseudo

{-| Filter elements of the given selector by pseudo selector functions. The
preferred syntax is to use one of the predefined functions from "Css.Pseudo".
-}
func : String -> (List String) -> Selector -> Selector
func = Css.Internal.Selector.func

-- ** Attribute-based refining.

{-| Given an attribute name and a selector, filter elements based on the presence
of that attribute. The preferred syntax is use one of the predefined attribute
Refinements from "Css.Attributes".
-}
withAttr : String -> Selector -> Selector
withAttr = Css.Internal.Selector.withAttr

{-| Filter elements based on the presence of a certain attribute with the
specified name and value.
-}
(.|@) : String -> String -> Selector -> Selector
(.|@) = Css.Internal.Selector.withAttrValue

{-| Filter elements based on the presence of a certain attribute that begins
with the specified value.
-}
(.|^) : String -> String -> Selector -> Selector
(.|^) = Css.Internal.Selector.withAttrValueBeginning

{-| Filter elements based on the presence of a certain attribute that ends
with the specified value.
-}
(.|$) : String -> String -> Selector -> Selector
(.|$) = Css.Internal.Selector.withAttrValueEnding

{-| Filter elements based on the presence of a certain attribute that contains
the specified value as a substring.
-}
(.|*) : String -> String -> Selector -> Selector
(.|*) = Css.Internal.Selector.withAttrValueContaining

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a space separated list.
-}
(.|~) : String -> String -> Selector -> Selector
(.|~) = Css.Internal.Selector.withAttrValueInSpacedList

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a hyphen separated list.
-}
(.|-) : String -> String -> Selector -> Selector
(.|-) = Css.Internal.Selector.withAttrValueInHyphenatedList

-------------------------------------------------------------------------------
-- * Rendering stylesheets to CSS strings.

{-| A type allowing various selectors to be composed together to indicate the
scope within which a set of rules applies.
-}
type alias Scope = Css.Internal.Stylesheet.SelectorScope

{-| Type to allow configuring print output of separators, newlines, etc.
-}
type alias Config = Css.Internal.Render.Config

{-| Render a stylesheet with the default configuration, using the pretty printer.
The stylesheet is passed in the form of a CssGenerator -- a function of Css to
Css -- which render will supply with an empty Css as the initial accumulator.
-}
render : CssGenerator a -> String
render = Css.Internal.Render.renderWith pretty []

{-| Render a stylesheet in compact format. The stylesheet is passed in the form
of a CssGenerator -- a function of Css to Css -- which render will supply with
an empty Css as the initial accumulator.
-}
renderCompact : CssGenerator a -> String
renderCompact = Css.Internal.Render.renderWith compact []

{-| Render a stylesheet with a custom configuration and an optional outer scope.
The stylesheet is a function of Css to Css, which render will supply with an
empty Css as the accumulator.
-}
renderWith : Config -> (List Scope) -> (Css -> Css) -> String
renderWith = Css.Internal.Render.renderWith

-------------------------------------------------------------------------------
-- * Special rules
-- ** The @media rule.

{-| A type representing the media type portion of a Css @media rule.
-}
type alias MediaType = Css.Internal.Stylesheet.MediaType

{-| A type representing the media features portion of a Css @media rule.
-}
type alias Feature = Css.Internal.Stylesheet.Feature

{-| A @media rule to apply a set of style rules when a media type and
feature queries apply.
-}
query : MediaType -> (List Feature) -> List (CssGenerator a) -> CssGenerator ()
query = Css.Internal.Stylesheet.query

{-| A  @media rule to apply a set of style rules when the media type and
feature queries do not apply.
-}
queryNot : MediaType -> (List Feature) -> List (CssGenerator a) -> CssGenerator ()
queryNot = Css.Internal.Stylesheet.queryNot

{-| A  @media rule to apply a set of style rules only when the media type and
feature queries apply.
-}
queryOnly : MediaType -> (List Feature) -> List (CssGenerator a) -> CssGenerator ()
queryOnly = Css.Internal.Stylesheet.queryOnly

-------------------------------------------------------------------------------
-- ** The @keyframes rule.

{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs.
-}
keyframes : String -> (List (Float, List (CssGenerator a))) -> CssGenerator ()
keyframes = Css.Internal.Stylesheet.keyframes

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules.
-}
keyframesFromTo : String -> List (CssGenerator a) -> List (CssGenerator a) -> CssGenerator ()
keyframesFromTo = Css.Internal.Stylesheet.keyframesFromTo

-------------------------------------------------------------------------------
-- ** The @font-face rule.

{-| Create a CSS @font-face rule from some css specifying font properties.
-}
fontFace : List (CssGenerator a) -> CssGenerator ()
fontFace = Css.Internal.Stylesheet.fontFace

-------------------------------------------------------------------------------
-- ** The @import rule.

{-| Create a CSS @import rule to import a CSS file from a URL.
-}
importUrl : String -> Css -> Css
importUrl = Css.Internal.Stylesheet.importUrl
