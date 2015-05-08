{-# LANGUAGE RecordWildCards #-}   
module TestPDV where

import Dicom.Network.Associate.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import Control.Monad
{-
buildPDV::PresentationDataValue
buildPDV = PDV{
                msgCtrlHeader = 1
              , msgFragment = [1,2,3,4,5,6,7]                  
              }
-}
buildPDVI::PresentationDataValueItem
buildPDVI = PDVI{
                  pdvItemHeader = PDVIH { pdvItemLength = 9}
                  , pdvContextID = 31
                  , msgCtrlHeader = 1
                  , msgFragment = [1,2,3,4,5,6,7]
                }
            
packPDVI::PresentationDataValueItem->BS.ByteString
packPDVI pdvi = BS.concat.BL.toChunks $ encode pdvi 

data PD = PD{len::Word8,wordlist::[Word8]} deriving Show
instance Binary PD where
  put PD{..} = do put len; mapM_ put wordlist
  get = do
           len <- get
           wordlist <- replicateM (fromIntegral len) get
           return PD{..}

