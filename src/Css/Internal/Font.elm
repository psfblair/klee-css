module Css.Internal.Font
  ( GenericFontFamily, GenericFontFamilyDescriptor
  , genericFontFamilyFactory, genericFontFamilyValue
  , FontSize, FontSizeDescriptor, fontSizeFactory, fontSizeValue
  , FontStyle, FontStyleDescriptor, fontStyleFactory, fontStyleValue
  , FontVariant, FontVariantDescriptor, fontVariantFactory, fontVariantValue
  , FontWeight, FontWeightDescriptor, fontWeightFactory, fontWeightValue
  , FontDescriptor, ComposedFontDescriptor
  , FontAlternative (..), FontComponents (..), fontFactory, fontValue
  ) where

import Css.Internal.Common exposing 
  (inheritValue, initialValue, normalValue, otherValue)
import Css.Internal.Property exposing 
  ( Value, toLiteral
  , stringValue, literalValue, maybeValue
  , commaListValue, spaceListValue
  , intersperse
  )
import Css.Internal.Size exposing (Size, sizeValue)

-------------------------------------------------------------------------------

-- The five generic font families.
-- <http://www.w3.org/TR/css3-fonts/#generic-font-families>.

type alias GenericFontFamilyDescriptor = 
  GenericFontFamilyFactory -> GenericFontFamily

type GenericFontFamily 
  = GenericFontFamily String
  | InitialGenericFontFamily
  | InheritGenericFontFamily
  | OtherGenericFontFamily Value

type alias GenericFontFamilyFactory =
  {
    family: String -> GenericFontFamily
  , initial: GenericFontFamily
  , inherit: GenericFontFamily
  , other: Value -> GenericFontFamily
  }


genericFontFamilyFactory : GenericFontFamilyFactory
genericFontFamilyFactory =
  {
    family str = GenericFontFamily str
  , initial = InitialGenericFontFamily
  , inherit = InheritGenericFontFamily
  , other val = OtherGenericFontFamily val
  }


genericFontFamilyValue : GenericFontFamily -> Value 
genericFontFamilyValue fontFamily =
  case fontFamily of
    GenericFontFamily str -> stringValue str
    InitialGenericFontFamily -> initialValue
    InheritGenericFontFamily -> inheritValue
    OtherGenericFontFamily val -> otherValue val
    
-------------------------------------------------------------------------------

type alias FontSizeDescriptor = FontSizeFactory -> FontSize

type FontSize 
  = FontSize String
  | InitialFontSize
  | InheritFontSize
  | OtherFontSize Value

type alias FontSizeFactory =
  {
    size: String -> FontSize
  , initial: FontSize
  , inherit: FontSize
  , other: Value -> FontSize
  }


fontSizeFactory : FontSizeFactory
fontSizeFactory =
  {
    size str = FontSize str
  , initial = InitialFontSize
  , inherit = InheritFontSize
  , other val = OtherFontSize val
  }


fontSizeValue : FontSize -> Value 
fontSizeValue fontSize =
  case fontSize of
    FontSize str -> stringValue str
    InitialFontSize -> initialValue
    InheritFontSize -> inheritValue
    OtherFontSize val -> otherValue val

-------------------------------------------------------------------------------

type alias FontStyleDescriptor = FontStyleFactory -> FontStyle

type FontStyle 
  = FontStyle String
  | NormalFontStyle
  | InheritFontStyle
  | InitialFontStyle
  | OtherFontStyle Value

type alias FontStyleFactory =
  {
    style: String -> FontStyle
  , initial: FontStyle
  , inherit: FontStyle
  , other: Value -> FontStyle
  }


fontStyleFactory : FontStyleFactory
fontStyleFactory =
  {
    style str = FontStyle str
  , initial = InitialFontStyle
  , inherit = InheritFontStyle
  , other val = OtherFontStyle val
  }


fontStyleValue : FontStyle -> Value 
fontStyleValue fontStyle =
  case fontStyle of
    FontStyle str -> stringValue str
    InitialFontStyle -> initialValue
    InheritFontStyle -> inheritValue
    OtherFontStyle val -> otherValue val

-------------------------------------------------------------------------------

type alias FontVariantDescriptor = FontVariantFactory -> FontVariant

type FontVariant 
  = FontVariant String
  | NormalFontVariant
  | InheritFontVariant
  | InitialFontVariant
  | OtherFontVariant Value

type alias FontVariantFactory =
  {
    variant: String -> FontVariant
  , normal: FontVariant
  , initial: FontVariant
  , inherit: FontVariant
  , other: Value -> FontVariant
  }

fontVariantFactory : FontVariantFactory
fontVariantFactory =
  {
    variant str = FontVariant str
  , normal = NormalFontVariant
  , initial = InitialFontVariant
  , inherit = InheritFontVariant
  , other val = OtherFontVariant val
  }

fontVariantValue : FontVariant -> Value 
fontVariantValue fontVariant =
  case fontVariant of
    FontVariant str -> stringValue str
    NormalFontVariant -> normalValue
    InitialFontVariant -> initialValue
    InheritFontVariant -> inheritValue
    OtherFontVariant val -> otherValue val

-------------------------------------------------------------------------------

type alias FontWeightDescriptor = FontWeightFactory -> FontWeight

type FontWeight 
  = FontWeight String
  | NormalFontWeight
  | InheritFontWeight
  | InitialFontWeight
  | OtherFontWeight Value


type alias FontWeightFactory =
  {
    weight: String -> FontWeight
  , normal: FontWeight
  , initial: FontWeight
  , inherit: FontWeight
  , other: Value -> FontWeight
  }


fontWeightFactory : FontWeightFactory
fontWeightFactory =
  {
    weight str = FontWeight str
  , normal = NormalFontWeight
  , initial = InitialFontWeight
  , inherit = InheritFontWeight
  , other val = OtherFontWeight val
  }


fontWeightValue : FontWeight -> Value 
fontWeightValue fontWeight =
  case fontWeight of
    FontWeight str -> stringValue str
    NormalFontWeight -> normalValue
    InitialFontWeight -> initialValue
    InheritFontWeight -> inheritValue
    OtherFontWeight val -> otherValue val

-------------------------------------------------------------------------------

{- We implement the `font` property to accept multiple value types. This lets 
us combine different font aspects into a shorthand syntax. According to the spec,
fonts have a mandatory part and an optional part.

font: font-style font-variant font-weight font-size/line-height font-family
      |caption|icon|menu|message-box|small-caption|status-bar|initial|inherit;
where in the first part, font-style font-variant font-weight are optional.

See <http://www.w3.org/TR/css3-fonts/#font-prop>

So we accommodate:
  font <| baseFont (px 15) ["Arial"] [sansSerif] 
        => font: 15px arial, sans-serif;
        
  baseFont (px 12) ["Georgia"] [Serif] 
    |> withLineHeight (px 30) 
    |> withWeight bold 
    |> withStyle italic 
    |> font
        => font: italic bold 12px/30px "Georgia", serif;
        
  font caption 
        => font: caption;
        
  font initial 
        => font: initial;
-}
type alias FontDescriptor a sz = FontFactory sz -> Font a sz

type alias Font a sz = { a | font : FontAlternative sz }
type alias WithComponents sz = { fontComponents : FontComponents sz }
type alias ComposedFont sz = Font (WithComponents sz) sz

type FontAlternative sz
  = NamedFont String
  | CompositeFont (FontComponents sz)
  | InitialFont
  | InheritFont

-- Font sizes can be absolute or relative
type FontComponents sz
  = BaseComponent (Size sz) (List String) (List GenericFontFamily)
  -- Line height case needs to be a second kind of leaf, to ease rendering
  | WithLineHeight (Size sz) (Size sz) (List String) (List GenericFontFamily)
  | WithWeight FontWeight (ComposedFont sz)
  | WithVariant FontVariant (ComposedFont sz)
  | WithStyle FontStyle (ComposedFont sz)

type alias FontFactory sz =
  { leaf : Size sz -> List String -> List GenericFontFamily -> ComposedFont sz
  , composite : (ComposedFont sz -> FontComponents sz) -> ComposedFont sz -> ComposedFont sz 
  , named : String -> Font {} sz
  , initial_ : Font {} sz
  , inherit_ : Font {} sz
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
  }
    
type alias ComposedFontDescriptor sz = FontFactory sz -> ComposedFont sz

fontValue : Font a sz -> Value
fontValue font =
  case font.font of
    NamedFont str -> stringValue str
    InitialFont -> initialValue
    InheritFont -> inheritValue
    CompositeFont fontComponents -> componentsToValue fontComponents

componentsToValue : FontComponents sz -> Value
componentsToValue fontComponents = 
  componentsToValueRecursive fontComponents Nothing Nothing Nothing


componentsToValueRecursive : FontComponents sz -> 
                             Maybe FontWeight ->
                             Maybe FontVariant -> 
                             Maybe FontStyle -> 
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
        let fontSizeValue = sizeValue fontSize
        in componentsLeafToValue fontSizeValue
                                 customFamilies 
                                 genericFamilies 
                                 maybeWeight 
                                 maybeVariant 
                                 maybeStyle
      WithLineHeight fontSize lineHeight customFamilies genericFamilies -> 
        -- should go to "italic bold 12px/30px Georgia, serif"
        let fontSizeValue = sizeValue lineHeight
            lineHeightValue = sizeValue fontSize
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
                        Maybe FontWeight ->
                        Maybe FontVariant -> 
                        Maybe FontStyle -> 
                        Value 
componentsLeafToValue sizeValue
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
            fontStyleVal = maybeValue fontStyleValue maybeStyle
            fontVariantVal = maybeValue fontVariantValue maybeVariant
            fontWeightVal = maybeValue fontWeightValue maybeWeight
        in spaceListValue identity 
            [ fontStyleVal
            , fontVariantVal
            , fontWeightVal
            , sizeValue
            , familiesValue] 