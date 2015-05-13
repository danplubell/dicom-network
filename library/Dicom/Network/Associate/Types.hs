{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Dicom.Network.Associate.Types where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Word
import GHC.Generics
import Data.Binary
import Data.Bits
import Control.Monad
{-
PDU Types
A-Associate-RQ 0x01
A-Associate-AC 0x02
A-Associate-RJ 0x03
P-Data-TF      0x04
A-Release-RQ   0x05
A-Release-RP   0x06
A-Abort        0x07

Item Types
Application Context Structure   0x10
Presentation Context            0x20
Abstract Syntax sub-item        0x30
Transfer Syntax sub-item        0x40
User Information Item           0x50
Presentation Context Item Field 0x21
-}

class (Binary a) => PDU a where
  packPDU::a -> BS.ByteString
  unpackPDU::BS.ByteString -> a  
  unpackPDU bs  = decode $ BL.fromChunks [bs]
  
getPDUTypeVal::PDUType -> Word8
getPDUTypeVal  p = fromIntegral $ fromEnum p

calPDULength :: (Binary a, Num b) => a -> b
calPDULength pdu = fromIntegral $ BL.length (encode pdu)-6 

data PDUType = A_ASSOCIATE_RQ|A_ASSOCIATE_AC|A_ASSOCIATE_RJ|P_DATA_TF|A_RELEASE_RQ|A_RELEASE_RP|A_ABORT|UNKNOWN_PDU deriving (Show,Eq,Ord)

instance Enum PDUType where
  toEnum 1 = A_ASSOCIATE_RQ
  toEnum 2 = A_ASSOCIATE_AC
  toEnum 3 = A_ASSOCIATE_RJ
  toEnum 4 = P_DATA_TF
  toEnum 5 = A_RELEASE_RQ
  toEnum 6 = A_RELEASE_RP
  toEnum 7 = A_ABORT
  toEnum 0 = UNKNOWN_PDU
  toEnum _ = UNKNOWN_PDU
  
  fromEnum UNKNOWN_PDU    = 0
  fromEnum A_ASSOCIATE_RQ = 1
  fromEnum A_ASSOCIATE_AC = 2
  fromEnum A_ASSOCIATE_RJ = 3
  fromEnum P_DATA_TF      = 4
  fromEnum A_RELEASE_RQ   = 5
  fromEnum A_RELEASE_RP   = 6
  fromEnum A_ABORT        = 7
  
  
data PDUHeader =PDUHeader{ pduType::Word8,pduHeaderReserved::Word8, pduLength::Word32} deriving (Show,Generic)

instance Binary PDUHeader

emptyHeader::PDUHeader
emptyHeader= PDUHeader 0 0 0

newAssociateRQPDU::AssociateRQPDU
newAssociateRQPDU = AssociateRQPDU {
    arqPDUHeader       = PDUHeader (getPDUTypeVal A_ASSOCIATE_RQ) 0 0
  , arqReserved        = 0
  , arqProtocolVersion = 0
  , calledAETitle      = ""
  , callingAETitle     = ""
  , arqVariableItems      = []                 
  , arqReserved2       = BL.replicate 32 0}

data AssociateRQPDU = AssociateRQPDU {
    arqPDUHeader       :: PDUHeader
  , arqProtocolVersion :: Word16
  , arqReserved        :: Word16
  , calledAETitle      :: String
  , callingAETitle     :: String
  , arqReserved2       :: BL.ByteString -- fixed length of 32 reserved bytes
  , arqVariableItems      :: [ARQItem] --includes Application Context Item, User Information Item, and Presentation Context.  These items can be in any order.  
  } deriving (Show,Generic)


instance Binary AssociateRQPDU          
instance PDU AssociateRQPDU where
  packPDU pdu = BS.concat $ BL.toChunks $ encode $ pdu { arqPDUHeader = (arqPDUHeader pdu)
                               {pduLength = calPDULength pdu}}
 
type CalledAETitle = String
type CallingAETitle = String
type ImplementationClassUID = String
type PDUTypeVal = Int

buildAssociateRQPDU::CalledAETitle ->CallingAETitle ->AssociateRQPDU
buildAssociateRQPDU toAE fromAE=
  AssociateRQPDU (PDUHeader (getPDUTypeVal A_ASSOCIATE_RQ) 0 0)
                   0
                   0
                   toAE
                   fromAE
                   (BL.replicate 32 0)
                   []
    
data AssociateACPDU = AssociateACPDU {
    accPDUHeader        :: PDUHeader
  , accProtocolVersion  :: Word16
  , accReserved         :: BL.ByteString -- 66 bytes reserved
  , accVariableItems    :: [[Word8]]
  } deriving (Show,Generic)

instance Binary AssociateACPDU
   
instance PDU AssociateACPDU where
  packPDU pdu = BS.concat . BL.toChunks $ encode $ pdu { accPDUHeader = (accPDUHeader pdu)
                              {pduLength = calPDULength pdu}}

buildAssociateACPDU::AssociateACPDU
buildAssociateACPDU = AssociateACPDU (PDUHeader (getPDUTypeVal A_ASSOCIATE_AC) 0 0)
                                     0
                                     (BL.replicate 66 0)
                                     [[]]
                                     
data AssociateRelRQPDU = AssociateRelRQPDU {
    arrqPDUHeader :: PDUHeader
  , arrqReserved  :: Word32
  } deriving (Show,Generic)
             
instance Binary AssociateRelRQPDU

instance PDU AssociateRelRQPDU where
  packPDU pdu = BS.concat . BL.toChunks $ encode $ pdu {arrqPDUHeader = (arrqPDUHeader pdu)
                              {pduLength = calPDULength pdu}}

buildAssociateRelRQ::AssociateRelRQPDU
buildAssociateRelRQ = AssociateRelRQPDU (PDUHeader (getPDUTypeVal A_RELEASE_RQ) 0 0) 0

data AssociateRelRPPDU = AssociateRelRPPDU {
    arrpPDUHeader :: PDUHeader
  , arrpReserved  :: Word32
  } deriving (Show,Generic)
             
instance Binary AssociateRelRPPDU

instance PDU AssociateRelRPPDU where
  packPDU pdu = BS.concat . BL.toChunks $ encode $ pdu {arrpPDUHeader = (arrpPDUHeader pdu)
                              {pduLength = calPDULength pdu}}

buildAssociateRelRP::AssociateRelRPPDU
buildAssociateRelRP = AssociateRelRPPDU (PDUHeader (getPDUTypeVal A_RELEASE_RP) 0 0) 0


{-
0 - reason-not-specified1 - unrecognized-PDU
2 - unexpected-PDU
3 - reserved
4 - unrecognized-PDU parameter
5 - unexpected-PDU parameter
6 - invalid-PDU-parameter value
-}


data AbortSource = AbortServiceProvider|AbortServiceUser|InvalidAbortSource
instance Enum AbortSource where
  fromEnum AbortServiceProvider = 2
  fromEnum AbortServiceUser     = 0
  fromEnum InvalidAbortSource   = 255

  toEnum 2 = AbortServiceProvider
  toEnum 0 = AbortServiceUser
  toEnum _ = InvalidAbortSource 

data AbortReason = AbortUnspecified | UnexpectedPDU | UnrecogPDUParam | UnexpectedPDUParam|InvalidPDUParamVal 
instance Enum AbortReason where
  fromEnum AbortUnspecified   = 0
  fromEnum UnexpectedPDU      = 2
  fromEnum UnrecogPDUParam    = 4
  fromEnum UnexpectedPDUParam = 5
  fromEnum InvalidPDUParamVal = 6

  toEnum 0 = AbortUnspecified
  toEnum 2 = UnexpectedPDU
  toEnum 4 = UnrecogPDUParam
  toEnum 5 = UnexpectedPDUParam
  toEnum 6 = InvalidPDUParamVal
  toEnum _ = AbortUnspecified  

data AbortPDU = AbortPDU {
    abortHeader    ::PDUHeader
  , abortReserved  ::Word16
  , abortSource    ::Word8
  , abortReason    ::Word8
  } deriving (Show,Generic)

instance Binary AbortPDU
instance PDU AbortPDU where
  packPDU pdu = BS.concat . BL.toChunks $ encode $ pdu {abortHeader = (abortHeader pdu)
                              {pduLength = calPDULength pdu}}
 

buildAbortPDU::AbortSource -> AbortReason -> AbortPDU
buildAbortPDU s r = AbortPDU (PDUHeader (getPDUTypeVal A_ABORT) 0 4)
                             0
                             (fromIntegral $ fromEnum r)
                             (fromIntegral $ fromEnum s)


{-
PDataTF contains a list of Presentation Data Value Items
Each Presentation Data Value Item contains one Presentation Data Value
Each Presentation Data Value contains a Command Message Fragment or a Data Message Fragment
-}
data PDataTF = PDataTF{
    pdataHeader :: PDUHeader
  , pdviList::[PresentationDataValueItem]
  } deriving (Show,Generic)
instance Binary PDataTF

data PDVItemHeader = PDVIH {
      pdvItemLength::Word32
    } deriving (Show,Generic)
instance Binary PDVItemHeader

data PresentationDataValueItem = PDVI{
      pdvItemHeader::PDVItemHeader
    , pdvContextID::Word8
    , msgCtrlHeader::Word8
    , msgFragment::[Word8]

    } deriving (Show,Generic)
instance Binary PresentationDataValueItem  where
  put p = do put (pdvItemHeader p)
             put (pdvContextID p)
             put (msgCtrlHeader p)
             mapM_ put (msgFragment p)
  get = do header <- get -- includes length from the context id to the end of the item 
           contextID <- get 
           ctrlHeader <- get
           fragment <- replicateM (fromIntegral (pdvItemLength header) - 2) get  -- need to subtract size of contextId and ctrlHeader
           return PDVI {pdvItemHeader = header ,pdvContextID = contextID, msgCtrlHeader = ctrlHeader, msgFragment= fragment}
             
--instance Binary PresentationDataValueItem
{-
data PresentationDataValue = PDV{
    msgCtrlHeader::Word8
  , msgFragment::[Word8]
  } deriving (Show,Generic)
instance Binary PresentationDataValue
-}
getPDVItemLength'::BS.ByteString -> Either String Word32
getPDVItemLength' bs = if BS.length bs /= 5 then Left "ByteString length does not match header length"
                      else Right (pdvItemLength (decode (BL.fromChunks  [BS.take 5 bs])::PDVItemHeader))

getPDVItemTotalLength::BS.ByteString -> Int
getPDVItemTotalLength bs = fromIntegral $ 4 + pdvItemLength (decode (BL.fromChunks [BS.take 4 bs])::PDVItemHeader)

getPDVItem::BS.ByteString -> PresentationDataValueItem
getPDVItem bs =  decode (BL.fromChunks [bs])::PresentationDataValueItem
                     

unpackPDVI::BS.ByteString -> PresentationDataValueItem
unpackPDVI bs =  getPDVItem $ BS.take itemLen bs 
                   where itemLen   = fromIntegral $ getPDVItemTotalLength bs
                         

unpackPDVIList ::BS.ByteString -> [PresentationDataValueItem]
unpackPDVIList bs | BS.null bs  = []
                  | otherwise   = let item = unpackPDVI bs
                                  in item:unpackPDVIList (BS.drop  (fromIntegral  (pdvItemLength $ pdvItemHeader item)+4) bs)
                        
{-Check to see if the PDV is a command fragment-}
isPDVCommand::PresentationDataValueItem -> Bool
isPDVCommand pdv = msgCtrlHeader pdv .&. 1 >  0

{-Check to see if the PDV is the last fragment-}
isPDVLastFragment :: PresentationDataValueItem -> Bool
isPDVLastFragment pdv = msgCtrlHeader pdv .&. 2 > 0 


data ARQItem = ApplicationContextItem
               {
                  acnType        ::Word8
                , acnReserved    ::Word8
                , acnLength      ::Word16
                , acnContextName ::String
               }
             | UserInformationItem
                 {
                     uiiType        ::Word8
                   , uiiReserved    ::Word8
                   , uiiLength      ::Word16
                   , uiiSubItemList ::[UserInformationSubItem]
                 }
             | PresentationContext
                 {
                     pcType              ::Word8
                   , pcLength            ::Word16
                   , pcIdentifier        ::Word8
                   , pcReserved1         ::Word8
                   , pcReserved2         ::Word8
                   , pcReserved3         ::Word8
                   , pcItemList          ::[PCItem]
                 }
             | UnknownARQItem        
             deriving (Show,Generic)

instance Binary ARQItem where
  put i = case i of
               ApplicationContextItem{..} ->
                 do put acnType
                    put acnReserved
                    put acnLength 
                    mapM_ put acnContextName
               UserInformationItem {..} ->
                 do put uiiType
                    put uiiReserved
                    put uiiLength
                    mapM_ put uiiSubItemList
               PresentationContext {..}->
                 do put pcType
                    put pcLength
                    put pcIdentifier
                    put pcReserved1
                    put pcReserved2
                    put pcReserved3
                    mapM_ put pcItemList
               UnknownARQItem -> putWord8 0
               
               
  get = do itemType <- getWord8
           case itemType of
             0x10 -> do acReserved <- get
                        aclength   <- get
                        acName     <- replicateM (fromIntegral aclength) get
                        return $ ApplicationContextItem itemType acReserved aclength acName                       
             0x20 -> populatePresentationContext itemType
             0x21 -> populatePresentationContext itemType
             0x50 -> return UnknownARQItem
             _    -> return UnknownARQItem
        where populatePresentationContext it  =
                do 
                   pclength     <- get
                   pcidentifier <- get
                   pcreserved1  <- get
                   pcreserved2  <- get
                   pcreserved3  <- get
                   pcitemlist   <- get
                   return $ PresentationContext it pclength pcidentifier pcreserved1 pcreserved2 pcreserved3 pcitemlist
                               
data PCItem = AbstractSyntax{
                 asItemType    ::Word8
               , asReserved    ::Word8
               , asItemLength  ::Word16
               , asName        ::String}
              | TransferSyntax{
                   tsItemType ::Word8
                 , tsReserved ::Word8
                 , tsLength   ::Word16
                 , tsName    ::String}
              | UnknownPCItem
              deriving (Show,Generic)
                       
instance Binary PCItem where
  put i = case i of
            AbstractSyntax {..} -> do put asItemType
                                      put asReserved
                                      put asItemLength
                                      mapM_ put asName
                               
            TransferSyntax {..} -> do put tsItemType
                                      put tsReserved
                                      put tsLength
                                      mapM_ put tsName
            UnknownPCItem       -> putWord8 0
  get = do
    itemType <- get
    case itemType of
      0x40 -> do tsreserved   <- get
                 tsitemlength <- get
                 tsname       <- replicateM (fromIntegral tsitemlength) get
                 return $ TransferSyntax itemType tsreserved tsitemlength tsname
      0x30 -> do asreserved   <- get
                 asitemlength <- get
                 asname       <- replicateM (fromIntegral asitemlength) get
                 return $ AbstractSyntax itemType asreserved asitemlength asname 
      _    -> return UnknownPCItem
data ItemHeader = ItemHeader { itemType::Word8, itemReserved::Word8, itemLength::Word16}
  deriving (Show,Generic)
instance Binary ItemHeader
data UserInformationSubItem = ImplementationClassUID        {   icItemHeader ::ItemHeader
                                                              , icUID        ::String}
                            | ImplementationVersionName     {   ivnItemHeader::ItemHeader
                                                              , ivn          ::String}
                            | MaximumLengthReceived         {   mlItemHeader ::ItemHeader
                                                              , mlMaxLength  ::Word32}
                            | UnknownSubItem
                                                            deriving (Show,Generic)
instance Binary UserInformationSubItem where
  put i = case i of
            ImplementationClassUID{..}    -> do put icItemHeader
                                                mapM_ put icUID
            ImplementationVersionName{..} -> do put ivnItemHeader
                                                mapM_ put ivn
                                                
            MaximumLengthReceived{..}     -> do put mlItemHeader
                                                put mlMaxLength
            UnknownSubItem                -> putWord8 0
  get = do           
          header <- get
          case itemType header of
            0x51 -> do
                      mlmaxlength <- get
                      return $ MaximumLengthReceived header mlmaxlength
            0x52 -> do
                      icUID <- replicateM (fromIntegral $ itemLength header) get
                      return $ ImplementationClassUID header icUID
            0x55 -> do
                      ivn <- replicateM (fromIntegral $ itemLength header) get
                      return $ ImplementationVersionName header ivn
            _    -> return UnknownSubItem          
            
