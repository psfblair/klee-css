module Css.Internal.Font
  ( GenericFontFamily, GenericFontFamilyDescriptor
  , genericFontFamilyFactory, genericFontFamilyValue
  , fontFamiliesValue
  , FontSizeDescriptor, fontSizeFactory
  , FontStyleDescriptor, fontStyleFactory
  , FontVariantDescriptor, fontVariantFactory
  , FontWeightDescriptor, fontWeightFactory
  , FontDescriptor, ComposedFontDescriptor
  , fontFactory, fontValue
  , addLineHeight, addWeight, addVariant, addStyle
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property
import Css.Internal.Geometry.Linear as Linear

-------------------------------------------------------------------------------

-- The five generic font families.
-- <http://www.w3.org/TR/css3-fonts/#generic-font-families>.

type alias GenericFontFamilyDescriptor = 
  GenericFontFamilyFactory -> GenericFontFamily

type GenericFontFamily 
  = GenericFontFamily String
  | OtherGenericFontFamily Property.Value

type alias GenericFontFamilyFactory =
  {
    family: String -> GenericFontFamily
  , other_: Property.Value -> GenericFontFamily
  }


genericFontFamilyFactory : GenericFontFamilyFactory
genericFontFamilyFactory =
  {
    family str = GenericFontFamily str
  , other_ val = OtherGenericFontFamily val
  }


genericFontFamilyValue : GenericFontFamily -> Property.Value 
genericFontFamilyValue fontFamily =
  case fontFamily of
    GenericFontFamily str -> Property.stringValue str
    OtherGenericFontFamily val -> Common.otherValue val

fontFamiliesValue : List String -> List GenericFontFamily -> Property.Value
fontFamiliesValue customFamilies genericFamilies =
  let customFamilyValues = 
        customFamilies 
        |> List.map Property.toLiteral 
        |> List.map Property.literalValue
      genericFamilyValues = 
        genericFamilies 
        |> List.map genericFontFamilyValue
      fontFamilyValues = customFamilyValues ++ genericFamilyValues
  in Property.commaListValue identity fontFamilyValues
    
-------------------------------------------------------------------------------

type alias FontSizeDescriptor = FontSizeFactory -> Property.Value

type alias FontSizeFactory =
  {
    size: String -> Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }


fontSizeFactory : FontSizeFactory
fontSizeFactory =
  {
    size str = Property.stringValue str
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias FontStyleDescriptor = FontStyleFactory -> Property.Value

type alias FontStyleFactory =
  {
    style: String -> Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , normal_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

fontStyleFactory : FontStyleFactory
fontStyleFactory =
  {
    style str = Property.stringValue str
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , normal_ = Common.normalValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias FontVariantDescriptor = FontVariantFactory -> Property.Value

type alias FontVariantFactory =
  {
    variant: String -> Property.Value
  , normal_ : Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }

fontVariantFactory : FontVariantFactory
fontVariantFactory =
  {
    variant str = Property.stringValue str
  , normal_ = Common.normalValue
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
  }

-------------------------------------------------------------------------------

type alias FontWeightDescriptor = FontWeightFactory -> Property.Value

type alias FontWeightFactory =
  {
    weight: String -> Property.Value
  , normal_ : Property.Value
  , initial_ : Property.Value
  , inherit_ : Property.Value
  , unset_ : Property.Value
  , other_ : Property.Value -> Property.Value
  }


fontWeightFactory : FontWeightFactory
fontWeightFactory =
  {
    weight str = Property.stringValue str
  , normal_ = Common.normalValue
  , initial_ = Common.initialValue
  , inherit_ = Common.inheritValue
  , unset_ = Common.unsetValue
  , other_ val = Common.otherValue val
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
  | OtherFont Property.Value

-- Font sizes can be absolute or relative
type FontComponents sz
  = BaseComponent Property.Value (List String) (List GenericFontFamily)
  | WithLineHeight Property.Value (ComposedFont sz)
  | WithWeight Property.Value (ComposedFont sz)
  | WithVariant Property.Value (ComposedFont sz)
  | WithStyle Property.Value (ComposedFont sz)

type alias FontFactory sz =
  { leaf : Linear.SizeDescriptor {} sz -> 
           List String -> 
           List GenericFontFamily -> 
           ComposedFont sz
  , composite : (ComposedFont sz -> FontComponents sz) -> 
                ComposedFont sz -> 
                ComposedFont sz 
  , named : String -> Font {} sz
  , initial_ : Font {} sz
  , inherit_ : Font {} sz
  , unset_  : Font {} sz
  , other_ : Property.Value -> Font {} sz
  }

fontFactory : FontFactory sz
fontFactory =
  { leaf sizeDescriptor customFonts genericFonts = 
      let sizeVal = sizeDescriptor Linear.nubSizeFactory 
          baseComponent = BaseComponent sizeVal customFonts genericFonts 
      in { font = CompositeFont baseComponent, fontComponents = baseComponent }
  , composite composer innerComposedFont =
      let newComponents = composer innerComposedFont
      in { font = CompositeFont newComponents, fontComponents = newComponents } 
  , named str   = { font = NamedFont str }
  , initial_    = { font = InitialFont   }
  , inherit_    = { font = InheritFont   }
  , unset_      = { font = UnsetFont     }
  , other_  val = { font = OtherFont val }
  }

fontValue : Font a sz -> Property.Value
fontValue font =
  case font.font of
    NamedFont str -> Property.stringValue str
    InitialFont   -> Common.initialValue
    InheritFont   -> Common.inheritValue
    UnsetFont     -> Common.unsetValue
    OtherFont val -> Common.otherValue val
    CompositeFont fontComponents -> componentsToValue fontComponents

addLineHeight : Property.Value -> 
                ComposedFontDescriptor sz -> 
                FontFactory sz -> 
                ComposedFont sz
addLineHeight lineHeight innerDescriptor factory =
  let innerFont = innerDescriptor factory
  in factory.composite (WithLineHeight lineHeight) innerFont

addWeight : Property.Value -> 
            ComposedFontDescriptor sz -> 
            FontFactory sz -> 
            ComposedFont sz
addWeight weight innerDescriptor factory =
  let innerFont = innerDescriptor factory
  in factory.composite (WithWeight weight) innerFont

addVariant: Property.Value -> 
            ComposedFontDescriptor sz -> 
            FontFactory sz -> 
            ComposedFont sz
addVariant variant innerDescriptor factory =
  let innerFont = innerDescriptor factory
  in factory.composite (WithVariant variant) innerFont

addStyle: Property.Value -> 
              ComposedFontDescriptor sz -> 
              FontFactory sz -> 
              ComposedFont sz
addStyle style innerDescriptor factory =   
  let innerFont = innerDescriptor factory
  in factory.composite (WithStyle style) innerFont

componentsToValue : FontComponents sz -> Property.Value
componentsToValue fontComponents = 
  componentsToValueRecursive fontComponents Nothing Nothing Nothing Nothing


componentsToValueRecursive : FontComponents sz -> 
                             Maybe Property.Value -> -- line height
                             Maybe Property.Value -> -- weight
                             Maybe Property.Value -> -- variant
                             Maybe Property.Value -> -- style
                             Property.Value
componentsToValueRecursive components maybeHeight maybeWeight maybeVariant maybeStyle =
  let recurse = componentsToValueRecursive
  in case components of
        -- If the FontComponents combinators are called more than once,
        -- the last (outer) one wins. So if it's already set we don't reset it.
        WithLineHeight lineHeight innerComposedFont ->
          let inner = innerComposedFont.fontComponents
          in case maybeHeight of
            Just _ -> 
              recurse inner maybeHeight maybeWeight maybeVariant maybeStyle
            Nothing -> 
              recurse inner (Just lineHeight) maybeWeight maybeVariant maybeStyle
        WithWeight weight innerComposedFont ->
          let inner = innerComposedFont.fontComponents
          in case maybeWeight of
            Just _ -> 
              recurse inner maybeHeight maybeWeight maybeVariant maybeStyle
            Nothing -> 
              recurse inner maybeHeight (Just weight) maybeVariant maybeStyle
        WithVariant variant innerComposedFont ->
          let inner = innerComposedFont.fontComponents
          in case maybeVariant of
            Just _ -> 
              recurse inner maybeHeight maybeWeight maybeVariant maybeStyle
            Nothing -> 
              recurse inner maybeHeight maybeWeight (Just variant) maybeStyle
        WithStyle style innerComposedFont ->
          let inner = innerComposedFont.fontComponents
          in case maybeStyle of
            Just _ -> 
              recurse inner maybeHeight maybeWeight maybeVariant maybeStyle
            Nothing -> 
              recurse inner maybeHeight maybeWeight maybeVariant (Just style)
        BaseComponent fontSize customFamilies genericFamilies -> 
          let familiesValue = fontFamiliesValue customFamilies genericFamilies
              sizesValue = 
                case maybeHeight of
                  -- should go to "italic bold 12px/30px Georgia, serif"
                  Just lineHeight -> 
                    Property.intersperse "/" [ fontSize, lineHeight ]
                  -- should go to "italic bold 15px arial, sans-serif"
                  Nothing -> fontSize

              allValues = 
                [ maybeStyle
                , maybeVariant
                , maybeWeight
                , Just(sizesValue)
                , Just(familiesValue)
                ] |> List.filterMap identity
                
          in Property.spaceListValue identity allValues
