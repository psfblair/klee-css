module Css.Internal.Font
  ( GenericFontFamily, GenericFontFamilyDescriptor
  , genericFontFamilyFactory, genericFontFamilyValue
  , FontFamilyDescriptor, fontFamilyFactory
  , fontFamiliesValue
  , FontSizeDescriptor, fontSizeFactory
  , NubFontStyleDescriptor, nubFontStyleFactory
  , FontStyleDescriptor, fontStyleFactory
  , NubFontVariantDescriptor, nubFontVariantFactory
  , FontVariantDescriptor, fontVariantFactory
  , NubFontWeightDescriptor, nubFontWeightFactory
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
  { family: String -> GenericFontFamily
  , other_: Property.Value -> GenericFontFamily
  }

genericFontFamilyFactory : GenericFontFamilyFactory
genericFontFamilyFactory =
  { family str = GenericFontFamily str
  , other_ val = OtherGenericFontFamily val
  }

genericFontFamilyValue : GenericFontFamily -> Property.Value 
genericFontFamilyValue fontFamily =
  case fontFamily of
    GenericFontFamily str -> Property.stringValue str
    OtherGenericFontFamily val -> Common.otherValue val

-------------------------------------------------------------------------------

type alias NubFontFamilyDescriptor rec = 
  NubFontFamilyFactory rec -> Property.Value

type alias FontFamilyDescriptor = FontFamilyFactory -> Property.Value

type alias NubFontFamilyFactory rec =
  { rec | customFamily: String -> Property.Value
        , families: List String -> 
                    List GenericFontFamilyDescriptor ->
                    Property.Value
        , other_: Property.Value -> Property.Value
  }

nubFontFamilyFactory : NubFontFamilyFactory {}
nubFontFamilyFactory = 
  { customFamily familyName = 
      familyName |> Property.toLiteral |> Property.literalValue
  , families customFamilyNames genericFamilyDescriptors = 
      let genericFamilyFrom descriptor = descriptor genericFontFamilyFactory
          genericFamilies = List.map genericFamilyFrom genericFamilyDescriptors
      in fontFamiliesValue customFamilyNames genericFamilies
  , other_ val = Common.otherValue val
  }
  
fontFamiliesValue : List String -> List GenericFontFamily -> Property.Value
fontFamiliesValue customFamilyNames genericFamilies =
  let customFamilyValues = 
        customFamilyNames 
        |> List.map Property.toLiteral 
        |> List.map Property.literalValue
      genericFamilyValues = 
        genericFamilies 
        |> List.map genericFontFamilyValue
      fontFamilyValues = customFamilyValues ++ genericFamilyValues
  in Property.commaListValue identity fontFamilyValues

type alias FontFamilyFactory = 
  NubFontFamilyFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))
  
fontFamilyFactory : FontFamilyFactory
fontFamilyFactory = Common.addCommonValues nubFontFamilyFactory
    
-------------------------------------------------------------------------------

type alias FontSizeDescriptor sz = FontSizeFactory sz -> Property.Value

type alias WithFontSize = { fontSize: String -> Property.Value }
  
type alias FontSizeFactory sz = Linear.BasicSizeFactory WithFontSize sz

fontSizeFactory : FontSizeFactory sz
fontSizeFactory = 
  let basicSizeFactory = Linear.basicSizeFactory
  in { basicSizeFactory | fontSize = \str -> Property.stringValue str }

-------------------------------------------------------------------------------
type alias NubFontStyleDescriptor rec = 
  NubFontStyleFactory rec -> Property.Value

type alias FontStyleDescriptor = FontStyleFactory -> Property.Value

type alias NubFontStyleFactory rec =
  { rec | style: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubFontStyleFactory : NubFontStyleFactory {}
nubFontStyleFactory =
  { style str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
type alias FontStyleFactory = 
  NubFontStyleFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value {}))))
  
fontStyleFactory : FontStyleFactory
fontStyleFactory = 
  let withCommon = Common.addCommonValues nubFontStyleFactory
  in { withCommon | normal_ = Common.normalValue }

-------------------------------------------------------------------------------
type alias NubFontVariantDescriptor rec = 
  NubFontVariantFactory rec -> Property.Value

type alias FontVariantDescriptor = FontVariantFactory -> Property.Value

type alias NubFontVariantFactory rec =
  { rec | variant: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }

nubFontVariantFactory : NubFontVariantFactory {}
nubFontVariantFactory =
  { variant str = Property.stringValue str
  , other_ val = Common.otherValue val
  }
  
type alias FontVariantFactory = 
  NubFontVariantFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value {}))))
  
fontVariantFactory : FontVariantFactory
fontVariantFactory = 
  let withCommon = Common.addCommonValues nubFontVariantFactory
  in { withCommon | normal_ = Common.normalValue }

-------------------------------------------------------------------------------
type alias NubFontWeightDescriptor rec = 
  NubFontWeightFactory rec -> Property.Value

type alias NubFontWeightFactory rec =
  { rec | weight: String -> Property.Value
        , other_ : Property.Value -> Property.Value
  }
  
nubFontWeightFactory : NubFontWeightFactory {}
nubFontWeightFactory =
  { weight str = Property.stringValue str
  , other_ val = Common.otherValue val
  }

type alias FontWeightDescriptor = FontWeightFactory -> Property.Value

type alias FontWeightFactory =
  NubFontWeightFactory 
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value 
          (Common.Normal Property.Value {}))))
  
fontWeightFactory : FontWeightFactory
fontWeightFactory = 
  let withCommon = Common.addCommonValues nubFontWeightFactory
  in { withCommon | normal_ = Common.normalValue }

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
  { leaf : Linear.NubSizeDescriptor {} sz -> 
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
