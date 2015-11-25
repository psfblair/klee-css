module Css.Internal.Typography.Font
  ( FontDescriptor, ComposedFontDescriptor
  , fontFactory, fontValue
  , addLineHeight, addWeight, addVariant, addStyle
  ) where

import Css.Internal.Common as Common
import Css.Internal.Geometry.Linear as Linear
import Css.Internal.Property as Property
import Css.Internal.Typography.Font.Family as Family

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
type alias FontDescriptor rec sz = FontFactory sz -> Font rec sz
type alias ComposedFontDescriptor sz = FontFactory sz -> ComposedFont sz

type alias Font rec sz = { rec | font : FontAlternative sz }
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
  = BaseComponent Property.Value (List String) (List Family.GenericFontFamily)
  | WithLineHeight Property.Value (ComposedFont sz)
  | WithWeight Property.Value (ComposedFont sz)
  | WithVariant Property.Value (ComposedFont sz)
  | WithStyle Property.Value (ComposedFont sz)

type alias FontFactory sz =
  { leaf : Linear.NubSizeDescriptor {} sz -> 
           List String -> 
           List Family.GenericFontFamily -> 
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

fontValue : Font rec sz -> Property.Value
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
          let familiesValue = 
                Family.fontFamiliesValue customFamilies genericFamilies
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
