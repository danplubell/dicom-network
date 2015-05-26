module ARQItemSpec (spec) where

import Test.Hspec
import Dicom.Network.Associate.Types
import Data.Binary
import qualified Data.ByteString.Lazy as BL

buildApplicationContextItem::ARQItem
buildApplicationContextItem = ApplicationContextItem {   acnHeader = ARQItemHeader ApplicationContextItemT 0 20 
                                                       , acnContextName = "12345678901234567890"
                                                     }
buildPresentationContextItem::ARQItemType -> ARQItem
buildPresentationContextItem t = PresentationContextItem {pcHeader= ARQItemHeader t 0 12
                                                        , pcIdentifier = 11
                                                        , pcReserved1  = 0
                                                        , pcReserved2  = 0
                                                        , pcReserved3  = 0
                                                        , pcItemList   = [AbstractSyntax (SubItemHeader AbstractSyntaxT 0 4) "0123"]
                                                        }

buildUserInformationItem::ARQItem
buildUserInformationItem = UserInformationItem { uiiHeader = ARQItemHeader UserInformationItemT 0 8
                                                 , uiiSubItemList = [MaximumLengthReceived (SubItemHeader MaximumLengthReceivedT 0 4) 32]
                                               }
spec::Spec
spec = do describe "Test binary decode/encode of ARQItems " $ do
            it "ApplicationContextItem" $ 
               let item = buildApplicationContextItem
                   itemBL = encode item
                in decode itemBL `shouldBe` item
            it "PresentationContextItem RQ" $ 
               let item = buildPresentationContextItem PresentationContextItemRQT
                   itemBL =  encode item
                in decode itemBL `shouldBe` item
            it "PresentationContextItem AC" $ 
               let item = buildPresentationContextItem PresentationContextItemACT
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
                let item = buildPresentationContextItem PresentationContextItemRQT
                    itemBL = encode item
                in unpackARQItemList itemBL `shouldBe` [item]
             it "Unpack User Information Item" $
                let item = buildUserInformationItem
                    itemBL = encode item
                in unpackARQItemList itemBL `shouldBe` [item]
             it "Unpack multiple" $
                let item1 = buildUserInformationItem
                    item2 = buildPresentationContextItem PresentationContextItemRQT
                    item3 = buildApplicationContextItem
                    itemBL = BL.concat [encode item1, encode item2, encode item3]
                    allItems = [item1, item2, item3]
                in unpackARQItemList itemBL `shouldBe` allItems
