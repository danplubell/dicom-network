module AssociateTypeSpec (spec) where

import Test.Hspec
import Dicom.Network.Associate.Types
import Data.Binary

buildTestAssociateRQ::AssociateRQPDU
buildTestAssociateRQ = AssociateRQPDU{
    arqPDUHeader          = PDUHeader A_ASSOCIATE_RQ 0 76
  , arqReserved        = 0
  , arqProtocolVersion = 0
  , calledAETitle      = "CALLEDAETITLE   "
  , callingAETitle     = "CALLINGAETITLE  "
  , arqVariableItems   = [buildARQItem]
  , arqReserved2        = replicate 32 0 }

buildARQItem::ARQItem
buildARQItem = ApplicationContextItem {
    acnHeader = ARQItemHeader ApplicationContextItemT 0 4
  , acnContextName = "abcd"}
               
spec::Spec
spec = describe "Test Associate Types" $ do
         it "AssociateRQ Type" $ 
           let item = buildTestAssociateRQ
               packed = encode item
            in decode packed `shouldBe` item
               
         it "AssociateRQ Type" $ do
               let packed = packPDU buildTestAssociateRQ 
               unpackPDU packed `shouldBe` buildTestAssociateRQ

