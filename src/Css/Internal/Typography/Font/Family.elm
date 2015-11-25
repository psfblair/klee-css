module Css.Internal.Typography.Font.Family
  ( GenericFontFamily, GenericFontFamilyDescriptor
  , genericFontFamilyFactory, genericFontFamilyValue
  , FontFamilyDescriptor, fontFamilyFactory
  , fontFamiliesValue
  ) where

import Css.Internal.Common as Common
import Css.Internal.Property as Property

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
