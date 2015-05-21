module ARQItemSpec (spec) where

import Test.Hspec
import Dicom.Network.Associate.Types
import Data.Binary

buildApplicationContextItem::ARQItem
buildApplicationContextItem = ApplicationContextItem {   acnHeader = ARQItemHeader 0x10 0 20 
                                                       , acnContextName = "12345678901234567890"
                                                     }
spec::Spec
spec = describe "Test binary decode/encode of ARQItems " $ 
            it "ApplicationContextItem" $ 
               let item = buildApplicationContextItem
                   itemBL = encode item
                in decode itemBL `shouldBe` item
              
