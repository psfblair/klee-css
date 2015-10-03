module Css (
  Css, Selector, Refinement, (-:), (?), (<?), (&), root, pop
  , star, element, (.,), (.>), (.+), (.|), (.|#), (.|.), pseudo, func, withAttr
  , (.|@), (.|^), (.|$), (.|*), (.|~), (.|-)
  , MediaType, Feature, query, queryNot, queryOnly
  , keyframes, keyframesFromTo, fontFace, importUrl
  , Scope, Config, render, renderWith, pretty, compact

) where

{-| A module for constructing Css in a typesafe way, and rendering the result
to a string (either pretty-printed or compact). This library is based on the
Haskell Clay CSS preprocessing framework, though some of the operators in the
selector DSL have been changed in order to avoid clashing with standard Elm
operators and for various other reasons.

The functions in this main module should be used in preference to those in
Css.Stylesheet, Css.Selector, Css.Render, which they export. Besides these,
the module Css.Property is also internal.

# Principal Types
@docs Css, Selector, Refinement

# Operators for aggregating style rules
@docs (-:), (?), (<?), (&), root, pop

# The selector language
@docs star, element, (.,), (.>), (.+), (.|), (.|#), (.|.), pseudo, func, withAttr,
      (.|@), (.|^), (.|$), (.|*), (.|~), (.|-)

# Rendering stylesheets to CSS strings
@docs Scope, Config, render, renderWith, pretty, compact

# Special rules
@docs MediaType, Feature, query, queryNot, queryOnly, keyframes, keyframesFromTo,
      fontFace, importUrl
-}

import Css.Stylesheet exposing (
  Css, Scope, (-:), (?), (<?), (&), root, pop
  , MediaType, Feature, query, queryNot, queryOnly
  , keyframes, keyframesFromTo, fontFace, importUrl
)

import Css.Selector exposing (
  Selector, Refinement, star, element, deep, child, adjacent, with, byId, byClass
  , pseudo, func, withAttr, withAttrValueBeginning, withAttrValueEnding
  , withAttrValueEnding, withAttrValueContaining, withAttrValueInSpacedList
  , withAttrValueInHyphenatedList
)

import Css.Render exposing (render, renderWith, pretty, compact, Config)

-------------------------------------------------------------------------------
-- * Principal types

{-| The type that collects the style rules.
-}
type alias Css = Css.Stylesheet.Css

{-|  A type to represent the selector in a CSS rule.
-}
type alias Selector = Css.Selector.Selector

{-|  A type to represent refinements on a selector; i.e., classes, ids, attributes
etc. that may be applied to a selector at a given level as filters (but not child
selectors).
-}
type alias Refinement = Css.Selector.Refinement

-------------------------------------------------------------------------------
-- * Operators for aggregating style rules.

{-| The fallback operator can be used to add style rules to the current context
for which there is no typed version available. Both the key and the value
are plain text values and rendered as is to the output CSS.
-}
(-:) : Css -> Key String -> String -> Css
(-:) = Css.Stylesheet.(-:)

{-| Assign a group of style rules to a selector. When the selector is nested inside an
outer scope it will be composed with `deep`, which maps to @sel1 sel2@ in CSS.
-}
(?) : Selector -> Css -> Css
(?) = Css.Stylesheet.(?)

{-| Assign a group of style rules to a selector. When the selector is nested inside
an outer scope it will be composed with `child`, which maps to @sel1 > sel2@ in CSS.
-}
(<?) : Selector -> Css -> Css
(<?) = Css.Stylesheet.(<?)

{-| Assign a group of style rules to a filter selector. When the selector is nested
inside an outer scope it will be composed with the `with` selector, which maps
to something like @sel#filter@ or @sel.filter@ in CSS depending on the filter.
-}
(&) : Refinement -> Css -> Css
(&) = Css.Stylesheet.(&)

{-| `root` is used to add style rules to the top scope.
-}
root : Selector -> Css -> Css
root = Css.Stylesheet.root

{-| `pop` is used to add style rules to selectors defined in an outer scope. The
counter specifies how far up the scope stack we want to add the rules.
-}
pop : Int -> Css -> Css
pop = Css.Stylesheet.pop

-------------------------------------------------------------------------------
-- * The selector language
-- ** Element selectors

{-| The star selector applies to all elements. Maps to @*@ in CSS.
-}
star : Selector
star = Css.Selector.star

{-| Creates a selector using its name. The preferred syntax is to use one of
the predefined elements from "Css.Elements".
-}
element : String -> Selector
element = Css.Selector.element

-- ** Composing and refining selectors

{-| The deep selector composer. Maps to @sel1 sel2@ in CSS.
-}
(.,) : Selector -> Selector -> Selector
(.,) = Css.Selector.deep

{-| The child selector composer. Maps to @sel1 > sel2@ in CSS.
-}
(.>) : Selector -> Selector -> Selector
(.>) = Css.Selector.child

{-| The adjacent selector composer. Maps to @sel1 + sel2@ in CSS.
-}
(.+) : Selector -> Selector -> Selector
(.+) = Css.Selector.adjacent

{-| The filter selector composer, which adds a filter to a selector. Maps to
something like @sel#filter@ or @sel.filter@ in CSS, depending on the filter.
-}
(.|) : Selector -> Refinement -> Selector
(.|) = Css.Selector.with

{-| Given an id and a selector, add an id filter to the selector.
-}
(.|#) : String -> Selector -> Selector
(.|#) = Css.Selector.byId

{-| Given a class name and a selector, add a class filter to the selector.
-}
(.|.) : String -> Selector -> Selector
(.|.) = Css.Selector.byClass

{-| Filter elements of the given selector by pseudo selector or pseudo class.
The preferred syntax is to use one of the predefined pseudo selectors from "Css.Pseudo".
-}
pseudo : String -> Selector -> Selector
pseudo = Css.Selector.pseudo

{-| Filter elements of the given selector by pseudo selector functions. The
preferred syntax is to use one of the predefined functions from "Css.Pseudo".
-}
func : String -> [String] -> Selector -> Selector
func = Css.Selector.func

-- ** Attribute-based refining.

{-| Given an attribute name and a selector, filter elements based on the presence
of that attribute. The preferred syntax is use one of the predefined attribute
Refinements from "Css.Attributes".
-}
withAttr : String -> Selector -> Selector
withAttr = Css.Selector.attr

{-| Filter elements based on the presence of a certain attribute with the
specified name and value.
-}
(.|@) : String -> String -> Selector -> Selector
(.|@) = Css.Selector.withAttrValue

{-| Filter elements based on the presence of a certain attribute that begins
with the specified value.
-}
(.|^) : String -> String -> Selector -> Selector
(.|^) = Css.Selector.withAttrValueBeginning

{-| Filter elements based on the presence of a certain attribute that ends
with the specified value.
-}
(.|$) : String -> String -> Selector -> Selector
(.|$) = Css.Selector.withAttrValueEnding

{-| Filter elements based on the presence of a certain attribute that contains
the specified value as a substring.
-}
(.|*) : String -> String -> Selector -> Selector
(.|*) = Css.Selector.withAttrValueContaining

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a space separated list.
-}
(.|~) : String -> String -> Selector -> Selector
(.|~) = Css.Selector.withAttrValueInSpacedList

{-| Filter elements based on the presence of a certain attribute that have the
specified value contained in a hyphen separated list.
-}
(.|-) : String -> String -> Selector -> Selector
(.|-) = Css.Selector.withAttrValueInHyphenatedList

-------------------------------------------------------------------------------
-- * Rendering stylesheets to CSS strings.

{-| A type allowing various selectors to be composed together to indicate the
scope within which a set of rules applies.
-}
type alias Scope = Css.Stylesheet.Scope

{-| Type to allow configuring print output of separators, newlines, etc.
-}
type alias Config = Css.Render.config

{-| Render a stylesheet with the default configuration. The pretty printer is
used by default.
-}
render : Css -> String
render = Css.Render.render

{-| Render a stylesheet with a custom configuration and an optional outer scope.
-}
renderWith : Config -> [Scope] -> Css -> String
renderWith = Css.Render.renderWith

{-| Configuration to print to a pretty human readable CSS output.
-}
pretty : Config
pretty = Css.Render.pretty

{-| Configuration to print to a compacted unreadable CSS output.
-}
compact : Config
compact = Css.Render.compact

-------------------------------------------------------------------------------
-- * Special rules
-- ** The @media rule.

{-| A type representing the media type portion of a Css @media rule.
-}
type alias MediaType = Css.Stylesheet.MediaType

{-| A type representing the media features portion of a Css @media rule.
-}
type alias Feature = Css.Stylesheet.Feature

{-| A @media rule to apply a set of style rules when a media type and
feature queries apply.
-}
query : MediaType -> [Feature] -> Css -> Css
query = Css.Stylesheet.query

{-| A  @media rule to apply a set of style rules when the media type and
feature queries do not apply.
-}
queryNot : MediaType -> [Feature] -> Css -> Css
queryNot = Css.Stylesheet.queryNot

{-| A  @media rule to apply a set of style rules only when the media type and
feature queries apply.
-}
queryOnly : MediaType -> [Feature] -> Css -> Css
queryOnly = Css.Stylesheet.queryOnly

-------------------------------------------------------------------------------
-- ** The @keyframes rule.

{-| Create a CSS @keyframes rule from an animation name and a list of
(percentage, css rules) pairs.
-}
keyframes : String -> [(Float, Css)] -> Css
keyframes = Css.Stylesheet.keyframes

{-| Create a CSS @keyframes rule from an animation name, some css starting rules,
and some css ending rules.
-}
keyframesFromTo : String -> Css -> Css -> Css
keyframesFromTo = Css.Stylesheet.keyframesFromTo

-------------------------------------------------------------------------------
-- ** The @font-face rule.

{-| Create a CSS @font-face rule from some css specifying font properties.
-}
fontFace : Css -> Css
fontFace Css.Stylesheet.fontFace

-------------------------------------------------------------------------------
-- ** The @import rule.

{-| Create a CSS @import rule to import a CSS file from a URL.
-}
importUrl : String -> Css
importUrl Css.Stylesheet.importUrl
