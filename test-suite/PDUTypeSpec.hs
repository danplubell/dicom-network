module PDUTypeSpec (spec) where

import Test.Hspec
import Dicom.Network.Associate.Types

spec::Spec
spec = describe "Test PDUType Values" $ do 
            it "Unknown PDU" $ do 
              getPDUTypeVal UNKNOWN_PDU    `shouldBe` 0
              toStructType 0                  `shouldBe` UNKNOWN_PDU
            it "A_ASSOCIATE_RQ" $ do
              getPDUTypeVal A_ASSOCIATE_RQ `shouldBe` 1
              toStructType 1                  `shouldBe` A_ASSOCIATE_RQ
            it "A_ASSOCIATE_AC" $ do
              getPDUTypeVal A_ASSOCIATE_AC `shouldBe` 2
              toStructType 2                  `shouldBe` A_ASSOCIATE_AC
            it "A_ASSOCIATE_RJ" $ do
              getPDUTypeVal A_ASSOCIATE_RJ `shouldBe` 3
              toStructType 3                  `shouldBe` A_ASSOCIATE_RJ
            it "P_DATA_TF" $ do
              getPDUTypeVal P_DATA_TF      `shouldBe` 4
              toStructType 4                  `shouldBe` P_DATA_TF
            it "A_RELEASE_RQ" $ do
              getPDUTypeVal A_RELEASE_RQ   `shouldBe` 5
              toStructType 5                  `shouldBe` A_RELEASE_RQ
            it "A_RELEASE_RP" $ do
              getPDUTypeVal A_RELEASE_RP   `shouldBe` 6
              toStructType 6                  `shouldBe` A_RELEASE_RP
            it "A_ABORT" $ do
              getPDUTypeVal A_ABORT        `shouldBe` 7
              toStructType 7                  `shouldBe` A_ABORT
            
