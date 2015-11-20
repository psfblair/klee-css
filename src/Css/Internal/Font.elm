module Css.Internal.Font
  ( GenericFontFamily, GenericFontFamilyDescriptor
  , genericFontFamilyFactory, genericFontFamilyValue
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
  -- Line height case needs to be a second kind of leaf, to ease rendering
  | WithLineHeight Property.Value Property.Value (List String) (List GenericFontFamily)
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

addLineHeight : ComposedFont sz -> 
                Linear.SizeDescriptor {} sz ->
                ComposedFont sz
addLineHeight fontWithComponents lineHeight = 
  case fontWithComponents.fontComponents of
  -- If withLineHeight is called twice, the later (outer) one wins, which 
  -- means that if this leaf has already been created, so don't touch it.
  WithLineHeight _ _ _ _ as leaf -> fontWithComponents
  BaseComponent size customFonts genericFonts -> 
    let components = 
      WithLineHeight size (lineHeight Linear.nubSizeFactory) customFonts genericFonts
    in { font = CompositeFont components, fontComponents = components }
  WithWeight weight innerComposedFont -> 
    let components = 
      WithWeight weight (addLineHeight innerComposedFont lineHeight)
    in { font = CompositeFont components, fontComponents = components }
  WithVariant variant innerComposedFont ->
    let components =
      WithVariant variant (addLineHeight innerComposedFont lineHeight)
    in { font = CompositeFont components, fontComponents = components }
  WithStyle style innerComposedFont -> 
    let components =
      WithStyle style (addLineHeight innerComposedFont lineHeight)
    in { font = CompositeFont components, fontComponents = components }

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
  componentsToValueRecursive fontComponents Nothing Nothing Nothing


componentsToValueRecursive : FontComponents sz -> 
                             Maybe Property.Value -> -- weight
                             Maybe Property.Value -> -- variant
                             Maybe Property.Value -> -- style
                             Property.Value
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
        componentsLeafToValue fontSize
                              customFamilies 
                              genericFamilies 
                              maybeWeight 
                              maybeVariant 
                              maybeStyle
      WithLineHeight fontSize lineHeight customFamilies genericFamilies -> 
        -- should go to "italic bold 12px/30px Georgia, serif"
        let sizes = Property.intersperse "/" [ fontSize, lineHeight ]
        in componentsLeafToValue sizes
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
componentsLeafToValue : Property.Value -> 
                        List String ->
                        List GenericFontFamily ->
                        Maybe Property.Value -> -- weight
                        Maybe Property.Value -> -- variant
                        Maybe Property.Value -> -- style
                        Property.Value 
componentsLeafToValue sizeVal
                      customFamilies 
                      genericFamilies 
                      maybeWeight 
                      maybeVariant 
                      maybeStyle =
  let customFamilyValues = 
        customFamilies 
        |> List.map Property.toLiteral 
        |> List.map Property.literalValue
      genericFamilyValues = 
        genericFamilies |> List.map genericFontFamilyValue
      familyValues = customFamilyValues ++ genericFamilyValues
      familiesValue = Property.commaListValue identity familyValues
      
      allValues = 
        [ maybeStyle
        , maybeVariant
        , maybeWeight
        , Just(sizeVal)
        , Just(familiesValue)
        ] |> List.filterMap identity
        
  in Property.spaceListValue identity allValues
