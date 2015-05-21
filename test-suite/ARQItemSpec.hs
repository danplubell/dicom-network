module ARQItemSpec (spec) where

import Test.Hspec
import Dicom.Network.Associate.Types
import Data.Binary
import qualified Data.ByteString.Lazy as BL

buildApplicationContextItem::ARQItem
buildApplicationContextItem = ApplicationContextItem {   acnHeader = ARQItemHeader 0x10 0 20 
                                                       , acnContextName = "12345678901234567890"
                                                     }
buildPresentationContextItem::Word8 -> ARQItem
buildPresentationContextItem t = PresentationContextItem {pcHeader= ARQItemHeader t 0 10
                                                        , pcIdentifier = 11
                                                        , pcReserved1  = 0
                                                        , pcReserved2  = 0
                                                        , pcReserved3  = 0
                                                        , pcItemList   = [1,2,3,4,5,6]
                                                        }

buildUserInformationItem::ARQItem
buildUserInformationItem = UserInformationItem { uiiHeader = ARQItemHeader 0x50 0 10
                                                 , uiiSubItemList = [0,1,2,3,4,5,6,7,8,9]
                                               }
spec::Spec
spec = do describe "Test binary decode/encode of ARQItems " $ do
            it "ApplicationContextItem" $ 
               let item = buildApplicationContextItem
                   itemBL = encode item
                in decode itemBL `shouldBe` item
            it "PresentationContextItem RQ" $ 
               let item = buildPresentationContextItem 0x20
                   itemBL =  encode item
                in decode itemBL `shouldBe` item
            it "PresentationContextItem AC" $ 
               let item = buildPresentationContextItem 0x21
                   itemBL =  encode item
                in decode itemBL `shouldBe` item
            it "UserInformationItem" $
               let item = buildUserInformationItem
                   itemBL = encode item
               in  decode itemBL `shouldBe` item
          describe "Test Unpack ARQ Item" $ do
             it "Unpack Application Context Item" $
                let item = buildApplicationContextItem
                    itemBL = encode item
                in unpackARQItemList itemBL `shouldBe` [item]
             it "Unpack Presentation Context" $ 
                let item = buildPresentationContextItem 0x20
                    itemBL = encode item
                in unpackARQItemList itemBL `shouldBe` [item]
             it "Unpack User Information Item" $
                let item = buildUserInformationItem
                    itemBL = encode item
                in unpackARQItemList itemBL `shouldBe` [item]
             it "Unpack multiple" $
                let item1 = buildUserInformationItem
                    item2 = buildPresentationContextItem 0x20
                    item3 = buildApplicationContextItem
                    itemBL = BL.concat [encode item1, encode item2, encode item3]
                    allItems = [item1, item2, item3]
                in unpackARQItemList itemBL `shouldBe` allItems
