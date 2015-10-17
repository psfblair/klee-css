module Css.Font
  (
  -- * Generic font property.

    font
  -- * Color.

  , fontColor
  , color

  -- * Font-family.

  , fontFamily
  , sansSerif
  , serif
  , monospace
  , cursive
  , fantasy

  -- * Font-size.

  , FontSize
  , fontSize
  , xxSmall, xSmall, small, medium, large, xLarge, xxLarge, smaller, larger

  -- * Font-style

  , FontStyle
  , fontStyle
  , italic, oblique

  -- * Font-variant.

  , FontVariant
  , fontVariant
  , smallCaps

  -- * Font-weight

  , FontWeight
  , fontWeight
  , bold, bolder, lighter
  , weight

  -- * Named fonts.

  , caption, icon, menu, messageBox, smallCaption, statusBar

  -- * Line-height.

  , lineHeight
  ) where

import Css.Internal.Property exposing 
  ( Value, ValueFactory, Literal (..), stringKey
  , stringValueFactory, literalValueFactory, maybeValueFactory, valueValueFactory
  , commaListValueFactory, spaceListValueFactory
  , intersperse
  ) 
import Css.Internal.Stylesheet exposing (key, PropertyRuleAppender)
import Css.Common exposing 
  ( Auto, Inherit, Normal, Initial, Other
  , inheritValueFactory, initialValueFactory, normalValueFactory, otherValueFactory
  )
import Css.Color exposing (Color, colorValueFactory)
import Css.Size exposing (Size, SizeDescriptor, sizeFactory, sizeValueFactory)

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
type alias Font a sz = { a | font : FontAlternative sz }
type alias WithComponents sz = { fontComponents : FontComponents sz }
type alias ComposedFont sz = Font sz (WithComponents sz)

type FontAlternative sz
  = NamedFont String
  | CompositeFont (FontComponents sz)
  | InitialFont
  | InheritFont
  | OtherFont Value

-- Font sizes can be absolute or relative
type FontComponents sz
  = BaseComponent (Size sz) (List String) (List GenericFontFamily)
  -- Line height case needs to be a second kind of leaf, to ease rendering
  | WithLineHeight (Size sz) (Size sz) (List String) (List GenericFontFamily)
  | WithWeight FontWeight (ComposedFont sz)
  | WithVariant FontVariant (ComposedFont sz)
  | WithStyle FontStyle (ComposedFont sz)

font : FontDescriptor a sz -> PropertyRuleAppender
font fontDescriptor = 
  key (stringKey "font") (fontDescriptor fontFactory) fontValueFactory

type alias FontDescriptor a sz = FontFactory sz -> Font a sz

type alias FontFactory sz =
  { named : String -> Font {} sz
  , leaf : Size sz -> List String -> List GenericFontFamily -> ComposedFont sz
  , composite : (ComposedFont sz -> FontComponents) -> ComposedFont sz -> ComposedFont sz
  , initial : Font {} sz
  , inherit : Font {} sz
  , other : Value -> Font {} sz
  }

fontFactory : FontFactory sz
fontFactory =
  { leaf size customFonts genericFonts = 
      let leaf = BaseComponent size customFonts genericFonts 
      in { font = CompositeFont leaf, fontComponents = baseFont }
  , composite composer innerCompositeFont =
      let newComponents = innerCompositeFont.fontComponents |> composer
      in { font = CompositeFont newComponents, fontComponents = newComponents }
  , named str = { font = NamedFont str}
  , initial = { font = InitialFontFamily }
  , inherit = { font = InheritFontFamily }
  , other val = { font = OtherFontFamily val }
  }
    
type alias ComposedFontDescriptor sz = FontFactory sz -> ComposedFont sz

{- Equivalent to
baseFont : SizeDescriptor a c -> 
           List String -> 
           List GenericFontFamily -> 
           FontFactory ComposedFont Font -> 
           Font
-}  
baseFont : SizeDescriptor (Size sz) sz -> 
           List String -> 
           List GenericFontFamily -> 
           ComposedFontDescriptor sz
baseFont sizeDescriptor customFonts genericFonts compositeFactory =
  let size = sizeDescriptor sizeFactory
  in compositeFactory.leaf size customFonts genericFonts

{- Equivalent to
withLineHeight :  SizeDescriptor -> 
                  (FontFactory ComposedFont Font -> Font) -> 
                  FontFactory ComposedFont Font  -> 
                  Font
-}
withLineHeight : SizeDescriptor (Size sz) sz -> 
                 ComposedFontDescriptor sz -> 
                 ComposedFontDescriptor sz
withLineHeight style lineHeightDescriptor compositeDescriptor compositeFactory =
   let components = (compositeDescriptor compositeFactory).fontComponents
       rewrapWithLineHeight fontComponents lineHeight = 
         case fontComponents of
           BaseComponent size customFonts genericFonts -> 
             WithLineHeight size lineHeight customFonts genericFonts
           -- If withLineHeight is called twice, the later (outer) one wins, which 
           -- means that if this leaf has already been created, so don't touch it.
           WithLineHeight _ _ _ _ as leaf -> leaf
           WithWeight weight compositeFont -> 
             WithWeight weight (rewrapWithLineHeight compositeFont lineHeight)
           WithVariant variant compositeFont ->
             WithVariant variant (rewrapWithLineHeight compositeFont lineHeight)
           WithStyle style compositeFont -> 
             WithStyle style (rewrapWithLineHeight compositeFont lineHeight)
   in rewrapWithLineHeight components (lineHeightDescriptor sizeFactory)
    
{- Equivalent to 
withWeight : FontWeight -> 
             (FontFactory ComposedFont Font -> Font) -> 
             FontFactory ComposedFont Font  -> 
             Font
-}
withWeight : FontWeight -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withWeight weight descriptor compositeFactory =
   let innerFont = descriptor compositeFactory
   in compositeFactory.composite (WithWeight weight) innerFont
  
{- Equivalent to 
withVariant : FontVariant -> 
              (FontFactory ComposedFont Font -> Font) -> 
              FontFactory ComposedFont Font  -> 
              Font
-}
withVariant : FontVariant -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withVariant variant descriptor compositeFactory =
   let innerFont = descriptor compositeFactory
   in compositeFactory.composite (WithVariant variant) innerFont

{- Equivalent to 
withStyle : FontStyle -> 
            (FontFactory ComposedFont Font -> Font) -> 
            FontFactory ComposedFont Font  -> 
            Font
-}
withStyle : FontStyle -> ComposedFontDescriptor sz -> ComposedFontDescriptor sz
withStyle style descriptor compositeFactory =
   let innerFont = descriptor compositeFactory
   in compositeFactory.composite (WithStyle style) innerFont
  
fontValueFactory : ValueFactory (Font a sz)
fontValueFactory =
  { value font = 
      case font.font of
        NamedFont str -> stringValueFactory.value str
        InitialFont -> initialValueFactory.initial
        InheritFont -> inheritValueFactory.inherit
        OtherFont val -> otherValueFactory.other val
        CompositeFont fontComponents -> componentsToValue fontComponents
  }
  
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
          Just weight -> 
            componentsToValueRecursive inner maybeWeight maybeVariant maybeStyle
          Nothing -> 
            componentsToValueRecursive inner weight maybeVariant maybeStyle
      WithVariant variant innerComposedFont ->
        let inner = innerComposedFont.fontComponents
        in case maybeVariant of
          Just variant -> 
            componentsToValueRecursive inner maybeWeight maybeVariant maybeStyle
          Nothing -> 
            componentsToValueRecursive inner maybeWeight variant maybeStyle
      WithStyle style innerComposedFont ->
        let inner = innerComposedFont.fontComponents
        in case maybeStyle of
          Just style -> 
            componentsToValueRecursive inner maybeWeight maybeVariant maybeStyle
          Nothing -> 
            componentsToValueRecursive inner maybeWeight maybeVariant style
      BaseComponent fontSize customFamilies genericFamilies -> 
        -- should go to "italic bold 15px arial, sans-serif"
        let fontSizeValue = sizeValueFactory.value fontSize
        in componentsLeafToValue fontSizeValue
                                 customFamilies 
                                 genericFamilies 
                                 maybeWeight 
                                 maybeVariant 
                                 maybeStyle
      WithLineHeight fontSize lineHeight customFamilies genericFamilies -> 
        -- should go to "italic bold 12px/30px Georgia, serif"
        let fontSizeValue = sizeValueFactory.value lineHeight
            lineHeightValue = sizeValueFactory.value fontSize
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
              customFamilies |> List.map Literal |> literalValueFactory.value
            genericFamilyValues = 
              genericFamilies |> List.map genericFontFamilyValueFactory
            familyValues = customFamilyValues ++ genericFamilyValues
            familiesValue = 
              (commaListValueFactory valueValueFactory).value familyValues
            fontStyleValue = 
              (maybeValueFactory fontStyleValueFactory).value maybeStyle
            fontVariantValue =
              (maybeValueFactory fontVariantValueFactory).value maybeVariant
            fontWeightValue =   
              (maybeValueFactory fontWeightValueFactory).value maybeWeight
        in (spaceListValueFactory valueValueFactory).value 
            [ fontStyleValue
            , fontVariantValue
            , fontWeightValue
            , sizeValue
            , familiesValue] 

caption : FontDescriptor {} sz
caption factory = factory.named "caption"

icon : FontDescriptor {} sz
icon factory = factory.named "icon"

menu : FontDescriptor {} sz
menu factory = factory.named "menu"

messageBox : FontDescriptor {} sz
messageBox factory = factory.named "message-box"

smallCaption : FontDescriptor {} sz
smallCaption factory = factory.named "small-caption"

statusBar : FontDescriptor {} sz
statusBar factory = factory.named "status-bar"

-------------------------------------------------------------------------------
color : Color -> PropertyRuleAppender
color colour = key "color" colour colorValueFactory

-- | An alias for `color`.
fontColor : Color -> PropertyRuleAppender
fontColor colour = key "color" colour colorValueFactory

-------------------------------------------------------------------------------
-- | The five generic font families.
-- <http://www.w3.org/TR/css3-fonts/#generic-font-families>.
type GenericFontFamily 
  = GenericFontFamily String
  | InitialFontFamily
  | InheritFontFamily
  | OtherFontFamily Value


type alias GenericFontFamilyDescriptor = 
  GenericFontFamilyFactory -> GenericFontFamily


sansSerif : GenericFontFamilyDescriptor 
sansSerif factory = factory.family "sans-serif"


serif : GenericFontFamilyDescriptor 
serif factory = factory.family "serif"


monospace : GenericFontFamilyDescriptor 
monospace factory = factory.family "monospace"


cursive : GenericFontFamilyDescriptor 
cursive factory = factory.family "cursive"


fantasy : GenericFontFamilyDescriptor
fantasy factory = factory.family "fantasy"


-- | The `fontFamily` style rule takes two lists of font families: zero or more
-- custom font-families and preferably one or more generic font families.
fontFamily : List String -> List GenericFontFamilyDescriptor -> PropertyRuleAppender
fontFamily customFamilies genericFamilies = 
  let customLiteralValues = 
        customFamilies |> List.map Literal |> List.map literalValueFactory.value
      genericValues = List.map genericFontFamilyValueFactory.value genericFamilies
      valueFactory = commaListValueFactory valueValueFactory
   in key "font-family" (customLiteralValues ++ genericValues) valueFactory


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
  , initial = InitialFontFamily
  , inherit = InheritFontFamily
  , other val = OtherFontFamily val
  }


genericFontFamilyValueFactory : ValueFactory GenericFontFamily
genericFontFamilyValueFactory =
  { value fontFamily =
      case fontFamily of
        GenericFontFamily str -> stringValueFactory.value str
        InitialFontFamily -> initialValueFactory.initial
        InheritFontFamily -> inheritValueFactory.inherit
        OtherFontFamily value -> otherValueFactory.other value
  }

-------------------------------------------------------------------------------

type FontSize 
  = FontSize String
  | InitialFontSize
  | InheritFontSize
  | OtherFontSize Value


type alias FontSizeDescriptor = FontSizeFactory -> FontSize


xxSmall : FontSizeDescriptor
xxSmall factory = factory.size "xx-small"


xSmall : FontSizeDescriptor 
xSmall factory = factory.size "x-small"


small : FontSizeDescriptor 
small factory = factory.size "small"


medium : FontSizeDescriptor
medium factory = factory.size "medium"


large : FontSizeDescriptor 
large factory = factory.size "large"


xLarge : FontSizeDescriptor 
xLarge factory = factory.size "x-large"


xxLarge : FontSizeDescriptor
xxLarge factory = factory.size "xx-large"


smaller : FontSizeDescriptor 
smaller factory = factory.size "smaller"


larger : FontSizeDescriptor
larger factory = factory.size "larger"


-- TODO Test that we can pass size descriptors here too.
fontSize : FontSizeDescriptor -> PropertyRuleAppender
fontSize sizeDescriptor = 
  key "font-size" (sizeDescriptor fontSizeFactory) fontSizeValueFactory


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


fontSizeValueFactory : ValueFactory FontSize
fontSizeValueFactory =
  { value fontSize =
      case fontSize of
        FontSize str -> stringValueFactory.value str
        InitialFontSize -> initialValueFactory.initial
        InheritFontSize -> inheritValueFactory.inherit
        OtherFontSize value -> otherValueFactory.other value
  }

-------------------------------------------------------------------------------

type FontStyle 
  = FontStyle String
  | NormalFontStyle
  | InheritFontStyle
  | InitialFontStyle
  | OtherFontStyle Value


type alias FontStyleDescriptor = FontStyleFactory -> FontStyle


italic : FontStyle
italic factory = factory.style "italic"


oblique : FontStyle
oblique factory = factory.style "oblique"


fontStyle : FontStyleDescriptor -> PropertyRuleAppender
fontStyle styleDescriptor = 
  key "font-style" (styleDescriptor fontStyleFactory) fontStyleValueFactory


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


fontStyleValueFactory : ValueFactory FontStyle
fontStyleValueFactory =
  { value fontStyle =
      case fontStyle of
        FontStyle str -> stringValueFactory.value str
        InitialFontStyle -> initialValueFactory.initial
        InheritFontStyle -> inheritValueFactory.inherit
        OtherFontStyle value -> otherValueFactory.other value
  }

-------------------------------------------------------------------------------

type FontVariant 
  = FontVariant String
  | NormalFontVariant
  | InheritFontVariant
  | InitialFontVariant
  | OtherFontVariant Value


type alias FontVariantDescriptor = FontVariantFactory -> FontVariant


smallCaps : FontVariantDescriptor
smallCaps factory = factory.variant "small-caps"


fontVariant : FontVariantDescriptor -> PropertyRuleAppender
fontVariant variantDescriptor = 
  key "font-variant" (variantDescriptor fontVariantFactory) fontVariantValueFactory


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


fontVariantValueFactory : ValueFactory FontVariant
fontVariantValueFactory =
  { value fontVariant =
      case fontVariant of
        FontVariant str -> stringValueFactory.value str
        NormalFontVariant -> normalValueFactory.normal
        InitialFontVariant -> initialValueFactory.initial
        InheritFontVariant -> inheritValueFactory.inherit
        OtherFontVariant value -> otherValueFactory.other value
  }

-------------------------------------------------------------------------------

type FontWeight 
  = FontWeight String
  | NormalFontWeight
  | InheritFontWeight
  | InitialFontWeight
  | OtherFontWeight Value


type alias FontWeightDescriptor = FontWeightFactory -> FontWeight


bold : FontWeightDescriptor
bold factory = factory.weight "bold"


bolder : FontWeightDescriptor 
bolder factory = factory.weight "bolder"


lighter : FontWeightDescriptor
lighter factory = factory.weight "lighter"


weight : Int -> FontWeightDescriptor
weight i factory = factory.weight (toString i)


fontWeight : FontWeightDescriptor -> PropertyRuleAppender
fontWeight descriptor = 
  key "font-weight" (descriptor fontWeightFactory) fontWeightValueFactory


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


fontWeightValueFactory : ValueFactory FontWeight
fontWeightValueFactory =
  { value fontWeight =
      case fontWeight of
        FontWeight str -> stringValueFactory.value str
        NormalFontWeight -> normalValueFactory.normal
        InitialFontWeight -> initialValueFactory.initial
        InheritFontWeight -> inheritValueFactory.inherit
        OtherFontWeight value -> otherValueFactory.other value
  }

-------------------------------------------------------------------------------

lineHeight : SizeDescriptor a c -> PropertyRuleAppender
lineHeight descriptor = 
  key "line-height" (descriptor sizeFactory) sizeValueFactory
