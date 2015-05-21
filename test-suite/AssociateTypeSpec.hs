module AssociateTypeSpec (spec) where

import Test.Hspec
import Dicom.Network.Associate.Types
import qualified Data.ByteString.Lazy as BL

buildTestAssociateRQ::AssociateRQPDU
buildTestAssociateRQ = AssociateRQPDU{
    arqPDUHeader          = PDUHeader (getPDUTypeVal A_ASSOCIATE_RQ) 0 0
  , arqReserved        = 0
  , arqProtocolVersion = 0
  , calledAETitle      = "CALLEDAETITLE   "
  , callingAETitle     = "CALLINGAETITLE  "
  , arqVariableItems   = [buildARQItem]
  , arqReserved2        = BL.replicate 32 0 }

buildARQItem::ARQItem
buildARQItem = ApplicationContextItem {
    acnHeader = ARQItemHeader 0x50 0 4
  , acnContextName = "abcd"}
spec::Spec
spec = describe "Test Associate Types" $ 
           it "AssociateRQ Type" $ do
               let packed = packPDU buildTestAssociateRQ 
               unpackPDU packed `shouldBe` buildTestAssociateRQ
