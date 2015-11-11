module Css.Internal.Font
  ( GenericFontFamily, GenericFontFamilyDescriptor
  , genericFontFamilyFactory, genericFontFamilyValue
  , FontSizeDescriptor, fontSizeFactory
  , FontStyleDescriptor, fontStyleFactory
  , FontVariantDescriptor, fontVariantFactory
  , FontWeightDescriptor, fontWeightFactory
  , FontDescriptor, ComposedFontDescriptor
  , FontAlternative (..), FontComponents (..), fontFactory, fontValue
  ) where

import Css.Internal.Common exposing 
  (inheritValue, initialValue, normalValue, otherValue, unsetValue)
import Css.Internal.Property exposing 
  ( Value, toLiteral
  , stringValue, literalValue, maybeValue
  , commaListValue, spaceListValue
  , intersperse
  )
import Css.Internal.Geometry.Linear as Linear

-------------------------------------------------------------------------------

-- The five generic font families.
-- <http://www.w3.org/TR/css3-fonts/#generic-font-families>.

type alias GenericFontFamilyDescriptor = 
  GenericFontFamilyFactory -> GenericFontFamily

type GenericFontFamily 
  = GenericFontFamily String
  | OtherGenericFontFamily Value

type alias GenericFontFamilyFactory =
  {
    family: String -> GenericFontFamily
  , other_: Value -> GenericFontFamily
  }


genericFontFamilyFactory : GenericFontFamilyFactory
genericFontFamilyFactory =
  {
    family str = GenericFontFamily str
  , other_ val = OtherGenericFontFamily val
  }


genericFontFamilyValue : GenericFontFamily -> Value 
genericFontFamilyValue fontFamily =
  case fontFamily of
    GenericFontFamily str -> stringValue str
    OtherGenericFontFamily val -> otherValue val
    
-------------------------------------------------------------------------------

type alias FontSizeDescriptor = FontSizeFactory -> Value

type alias FontSizeFactory =
  {
    size: String -> Value
  , initial_ : Value
  , inherit_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }


fontSizeFactory : FontSizeFactory
fontSizeFactory =
  {
    size str = stringValue str
  , initial_ = initialValue
  , inherit_ = inheritValue
  , unset_ = unsetValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias FontStyleDescriptor = FontStyleFactory -> Value

type alias FontStyleFactory =
  {
    style: String -> Value
  , initial_ : Value
  , inherit_ : Value
  , normal_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }

fontStyleFactory : FontStyleFactory
fontStyleFactory =
  {
    style str = stringValue str
  , initial_ = initialValue
  , inherit_ = inheritValue
  , normal_ = normalValue
  , unset_ = unsetValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias FontVariantDescriptor = FontVariantFactory -> Value

type alias FontVariantFactory =
  {
    variant: String -> Value
  , normal_ : Value
  , initial_ : Value
  , inherit_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }

fontVariantFactory : FontVariantFactory
fontVariantFactory =
  {
    variant str = stringValue str
  , normal_ = normalValue
  , initial_ = initialValue
  , inherit_ = inheritValue
  , unset_ = unsetValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

type alias FontWeightDescriptor = FontWeightFactory -> Value

type alias FontWeightFactory =
  {
    weight: String -> Value
  , normal_ : Value
  , initial_ : Value
  , inherit_ : Value
  , unset_ : Value
  , other_ : Value -> Value
  }


fontWeightFactory : FontWeightFactory
fontWeightFactory =
  {
    weight str = stringValue str
  , normal_ = normalValue
  , initial_ = initialValue
  , inherit_ = inheritValue
  , unset_ = unsetValue
  , other_ val = otherValue val
  }

-------------------------------------------------------------------------------

{- We implement the `font` property to accept multiple value types. This lets 
us combine different font aspects into a shorthand syntax. According to the spec,
fonts have a mandatory part and an optional part.

font: font-style font-variant font-weight font-size/line-height font-family
      |caption|icon|menu|message-box|small-caption|status-bar|initial|inherit;
where in the first part, font-style font-variant font-weight are optional.

See <http://www.w3.org/TR/css3-fonts/#font-prop>

So our DSL looks like this:

    font <| aFont (px 15) ["Arial"] [sansSerif] 
        => font: 15px arial, sans-serif;

or:        
    aFont (px 12) ["Georgia"] [Serif] 
    |> withLineHeight (px 30) 
    |> withWeight bold 
    |> withStyle italic 
    |> font
        => font: italic bold 12px/30px "Georgia", serif;

or:        
    font initial 
        => font: initial;
-}
type alias FontDescriptor a sz = FontFactory sz -> Font a sz
type alias ComposedFontDescriptor sz = FontFactory sz -> ComposedFont sz

type alias Font a sz = { a | font : FontAlternative sz }
type alias WithComponents sz = { fontComponents : FontComponents sz }
type alias ComposedFont sz = Font (WithComponents sz) sz

type FontAlternative sz
  = NamedFont String
  | CompositeFont (FontComponents sz)
  | InitialFont
  | InheritFont
  | UnsetFont
  | OtherFont Value

-- Font sizes can be absolute or relative
type FontComponents sz
  = BaseComponent (Linear.Size sz) (List String) (List GenericFontFamily)
  -- Line height case needs to be a second kind of leaf, to ease rendering
  | WithLineHeight (Linear.Size sz) (Linear.Size sz) (List String) (List GenericFontFamily)
  | WithWeight Value (ComposedFont sz)
  | WithVariant Value (ComposedFont sz)
  | WithStyle Value (ComposedFont sz)

type alias FontFactory sz =
  { leaf : Linear.Size sz -> List String -> List GenericFontFamily -> ComposedFont sz
  , composite : (ComposedFont sz -> FontComponents sz) -> ComposedFont sz -> ComposedFont sz 
  , named : String -> Font {} sz
  , initial_ : Font {} sz
  , inherit_ : Font {} sz
  , unset_  : Font {} sz
  , other_ : Value -> Font {} sz
  }

fontFactory : FontFactory sz
fontFactory =
  { leaf size customFonts genericFonts = 
      let baseComponent = BaseComponent size customFonts genericFonts 
      in { font = CompositeFont baseComponent, fontComponents = baseComponent }
  , composite composer innerComposedFont =
      let newComponents = composer innerComposedFont
      in { font = CompositeFont newComponents, fontComponents = newComponents } 
  , named str  = { font = NamedFont str}
  , initial_   = { font = InitialFont }
  , inherit_   = { font = InheritFont }
  , unset_   = { font = UnsetFont }
  , other_  val = { font = OtherFont val }
  }

fontValue : Font a sz -> Value
fontValue font =
  case font.font of
    NamedFont str -> stringValue str
    InitialFont -> initialValue
    InheritFont -> inheritValue
    UnsetFont -> unsetValue
    OtherFont val -> otherValue val
    CompositeFont fontComponents -> componentsToValue fontComponents

componentsToValue : FontComponents sz -> Value
componentsToValue fontComponents = 
  componentsToValueRecursive fontComponents Nothing Nothing Nothing


componentsToValueRecursive : FontComponents sz -> 
                             Maybe Value -> -- weight
                             Maybe Value -> -- variant
                             Maybe Value -> -- style
                             Value
componentsToValueRecursive components maybeWeight maybeVariant maybeStyle =
  case components of
      -- If the FontComponents combinators are called more than once,
      -- the last (outer) one wins. So if it's already set we don't reset it.
      WithWeight weight innerComposedFont ->
        let inner = innerComposedFont.fontComponents
        in case maybeWeight of
          Just _ -> 
            componentsToValueRecursive inner maybeWeight maybeVariant maybeStyle
          Nothing -> 
            componentsToValueRecursive inner (Just weight) maybeVariant maybeStyle
      WithVariant variant innerComposedFont ->
        let inner = innerComposedFont.fontComponents
        in case maybeVariant of
          Just _ -> 
            componentsToValueRecursive inner maybeWeight maybeVariant maybeStyle
          Nothing -> 
            componentsToValueRecursive inner maybeWeight (Just variant) maybeStyle
      WithStyle style innerComposedFont ->
        let inner = innerComposedFont.fontComponents
        in case maybeStyle of
          Just _ -> 
            componentsToValueRecursive inner maybeWeight maybeVariant maybeStyle
          Nothing -> 
            componentsToValueRecursive inner maybeWeight maybeVariant (Just style)
      BaseComponent fontSize customFamilies genericFamilies -> 
        -- should go to "italic bold 15px arial, sans-serif"
        let fontSizeValue = Linear.sizeValue fontSize
        in componentsLeafToValue fontSizeValue
                                 customFamilies 
                                 genericFamilies 
                                 maybeWeight 
                                 maybeVariant 
                                 maybeStyle
      WithLineHeight fontSize lineHeight customFamilies genericFamilies -> 
        -- should go to "italic bold 12px/30px Georgia, serif"
        let fontSizeValue = Linear.sizeValue lineHeight
            lineHeightValue = Linear.sizeValue fontSize
            sizes = intersperse "/" [ fontSizeValue, lineHeightValue ]
        in componentsLeafToValue fontSizeValue
                                 customFamilies 
                                 genericFamilies 
                                 maybeWeight 
                                 maybeVariant 
                                 maybeStyle

{- font-style font-variant font-weight font-size font-family
or
   font-style font-variant font-weight font-size/line-height font-family
where font-family is comma-separated
-}
componentsLeafToValue : Value -> 
                        List String ->
                        List GenericFontFamily ->
                        Maybe Value -> -- weight
                        Maybe Value -> -- variant
                        Maybe Value -> -- style
                        Value 
componentsLeafToValue sizeVal
                      customFamilies 
                      genericFamilies 
                      maybeWeight 
                      maybeVariant 
                      maybeStyle =
  let customFamilyValues = 
        customFamilies |> List.map toLiteral |> List.map literalValue
      genericFamilyValues = 
        genericFamilies |> List.map genericFontFamilyValue
      familyValues = customFamilyValues ++ genericFamilyValues
      familiesValue = commaListValue identity familyValues
      
      allValues = 
        [ maybeStyle
        , maybeVariant
        , maybeWeight
        , Just(sizeVal)
        , Just(familiesValue)
        ] |> List.filterMap identity
        
  in spaceListValue identity allValues
