module Css.Internal.Background.Attachment
  ( BackgroundAttachmentDescriptor, backgroundAttachmentFactory
  ) where

import Css.Internal.Common as Common 
import Css.Internal.Property as Property 

-------------------------------------------------------------------------------

type alias BackgroundAttachmentDescriptor = 
  BackgroundAttachmentFactory -> Property.Value

type alias NubBackgroundAttachmentFactory rec =
  { rec | bgAttachment : String -> Property.Value 
        , other_ : Property.Value -> Property.Value 
  }  

type alias BackgroundAttachmentFactory =
  NubBackgroundAttachmentFactory
    (Common.Initial Property.Value
      (Common.Inherit Property.Value
        (Common.Unset Property.Value {})))

nubBackgroundAttachmentFactory : NubBackgroundAttachmentFactory {}
nubBackgroundAttachmentFactory =
  { bgAttachment str = Property.stringValue str
  , other_ val = Common.otherValue val 
  }  
  
backgroundAttachmentFactory : BackgroundAttachmentFactory
backgroundAttachmentFactory = 
  Common.addCommonValues nubBackgroundAttachmentFactory
