{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Dicom.Network.Associate.Types where

import qualified Data.ByteString.Lazy as BL
--import qualified Data.ByteString as BS
import Data.Word
import GHC.Generics
import Data.Binary
import Data.Bits
import Control.Monad
import Data.Int

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
  packPDU::a -> BL.ByteString
  unpackPDU::BL.ByteString -> a  
  unpackPDU   = decode 

class PDUType a  where
  toPDUType   :: Word8 -> a
  fromPDUType :: a -> Word8  

getPDUTypeVal::PDUTypes -> Word8
getPDUTypeVal   = fromPDUType 

{-
Attempts to get the PDU type from a given bytestring.
The PDU type is the first byte int the bytestring
-}
getPDUType::BL.ByteString -> PDUTypes  
getPDUType  bs = if BL.null bs then UNKNOWN_PDU
                   else toPDUType (head $ BL.unpack bs)

calPDULength :: (Binary a, Num b) => a -> b
calPDULength pdu = fromIntegral $ BL.length (encode pdu)-6 

data PDUTypes = A_ASSOCIATE_RQ|A_ASSOCIATE_AC|A_ASSOCIATE_RJ|P_DATA_TF|A_RELEASE_RQ|A_RELEASE_RP|A_ABORT|UNKNOWN_PDU deriving (Show,Eq,Ord)

instance PDUType PDUTypes where
  toPDUType 1 = A_ASSOCIATE_RQ
  toPDUType 2 = A_ASSOCIATE_AC
  toPDUType 3 = A_ASSOCIATE_RJ
  toPDUType 4 = P_DATA_TF
  toPDUType 5 = A_RELEASE_RQ
  toPDUType 6 = A_RELEASE_RP
  toPDUType 7 = A_ABORT
  toPDUType 0 = UNKNOWN_PDU
  toPDUType _ = UNKNOWN_PDU
  
  fromPDUType UNKNOWN_PDU    = 0
  fromPDUType A_ASSOCIATE_RQ = 1
  fromPDUType A_ASSOCIATE_AC = 2
  fromPDUType A_ASSOCIATE_RJ = 3
  fromPDUType P_DATA_TF      = 4
  fromPDUType A_RELEASE_RQ   = 5
  fromPDUType A_RELEASE_RP   = 6
  fromPDUType A_ABORT        = 7
  
  
data PDUHeader =PDUHeader{ pduType::Word8, pduHeaderReserved::Word8, pduLength::Word32} deriving (Show,Generic,Eq)

instance Binary PDUHeader

newAssociateRQPDU::AssociateRQPDU
newAssociateRQPDU = AssociateRQPDU {
    arqPDUHeader       = PDUHeader (getPDUTypeVal A_ASSOCIATE_RQ) 0 0
  , arqReserved        = 0
  , arqProtocolVersion = 0
  , calledAETitle      = ""
  , callingAETitle     = ""
  , arqVariableItems   = []                 
  , arqReserved2       = BL.replicate 32 0}

data AssociateRQPDU = AssociateRQPDU {
    arqPDUHeader       :: PDUHeader
  , arqProtocolVersion :: Word16
  , arqReserved        :: Word16
  , calledAETitle      :: String
  , callingAETitle     :: String
  , arqReserved2       :: BL.ByteString -- fixed length of 32 reserved bytes
  , arqVariableItems   :: [ARQItem] --includes Application Context Item, User Information Item, and Presentation Context.  These items can be in any order.  
  } deriving (Show,Generic,Eq)


instance Binary AssociateRQPDU where
  put a = do put (arqPDUHeader a)
             put (arqProtocolVersion a)
             put (arqReserved a)
             mapM_ put (take 16 $ calledAETitle a)
             mapM_ put (take 16 $ callingAETitle a)
             put (arqReserved2 a)
             mapM_ put (arqVariableItems a)
  get = do arqHeader      <- get
           arqprotocolv   <- get
           arqreserved    <- get
           calledaetitle  <- replicateM 16 get
           callingaetitle <- replicateM 16 get
           arqreserved2   <- get
           return $ AssociateRQPDU arqHeader arqprotocolv arqreserved calledaetitle callingaetitle arqreserved2 []



instance PDU AssociateRQPDU where
  packPDU pdu = encode $ pdu { arqPDUHeader = (arqPDUHeader pdu)
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
  packPDU pdu = encode $ pdu { accPDUHeader = (accPDUHeader pdu)
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
  packPDU pdu = encode $ pdu {arrqPDUHeader = (arrqPDUHeader pdu)
                              {pduLength = calPDULength pdu}}


buildAssociateRelRQ::AssociateRelRQPDU
buildAssociateRelRQ = AssociateRelRQPDU (PDUHeader (getPDUTypeVal A_RELEASE_RQ) 0 0) 0

data AssociateRelRPPDU = AssociateRelRPPDU {
    arrpPDUHeader :: PDUHeader
  , arrpReserved  :: Word32
  } deriving (Show,Generic)
             
instance Binary AssociateRelRPPDU


instance PDU AssociateRelRPPDU where
  packPDU pdu = encode $ pdu {arrpPDUHeader = (arrpPDUHeader pdu)
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
  packPDU pdu = encode $ pdu {abortHeader = (abortHeader pdu)
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
getPDVItemLength'::BL.ByteString -> Either String Word32
getPDVItemLength' bs = if BL.length bs /= 5 then Left "ByteString length does not match header length"
                      else Right (pdvItemLength (decode (BL.take 5 bs)::PDVItemHeader))

getPDVItemTotalLength::BL.ByteString -> Int
getPDVItemTotalLength bs = fromIntegral $ 4 + pdvItemLength (decode (BL.take 4 bs)::PDVItemHeader)

getPDVItem::BL.ByteString -> PresentationDataValueItem
getPDVItem bs =  decode bs::PresentationDataValueItem
                     

unpackPDVI::BL.ByteString -> PresentationDataValueItem
unpackPDVI bs =  getPDVItem $ BL.take itemLen bs 
                   where itemLen   = fromIntegral $ getPDVItemTotalLength bs
                         

unpackPDVIList ::BL.ByteString -> [PresentationDataValueItem]
unpackPDVIList bs | BL.null bs  = []
                  | otherwise   = let item = unpackPDVI bs
                                  in item:unpackPDVIList (BL.drop  (fromIntegral  (pdvItemLength $ pdvItemHeader item)+4) bs)
                        
{-Check to see if the PDV is a command fragment-}
isPDVCommand::PresentationDataValueItem -> Bool
isPDVCommand pdv = msgCtrlHeader pdv .&. 1 >  0

{-Check to see if the PDV is the last fragment-}
isPDVLastFragment :: PresentationDataValueItem -> Bool
isPDVLastFragment pdv = msgCtrlHeader pdv .&. 2 > 0 

data ARQItemHeader = ARQItemHeader
                     {
                        arqItemType       ::Word8
                     ,  arqItemReserved   ::Word8
                     ,  arqItemLength     ::Word16
                     } deriving (Show, Eq, Generic)
instance Binary ARQItemHeader

class ARQItemType a where
  getARQHeader::a -> ARQItemHeader

data ARQItem = ApplicationContextItem
               {
                 acnHeader      ::ARQItemHeader
               , acnContextName ::String
               }
             | UserInformationItem
               {
                 uiiHeader      ::ARQItemHeader
               , uiiSubItemList ::[Word8]
               }
             | PresentationContextItem
               {
                 pcHeader            ::ARQItemHeader
               , pcIdentifier        ::Word8
               , pcReserved1         ::Word8
               , pcReserved2         ::Word8
               , pcReserved3         ::Word8
               , pcItemList          ::[Word8]
               }
             | UnknownARQItem        
             deriving (Show,Generic,Eq)

instance ARQItemType ARQItem where
  getARQHeader ApplicationContextItem{..} = acnHeader 
  getARQHeader UserInformationItem{..}    = uiiHeader
  getARQHeader PresentationContextItem{..}    = pcHeader
  getARQHeader UnknownARQItem             = ARQItemHeader 0 0 0
  
instance Binary ARQItem where
  put i = case i of
               ApplicationContextItem{..} ->
                 do put acnHeader
                    mapM_ put acnContextName
               UserInformationItem {..} ->
                 do put uiiHeader
                    mapM_ put uiiSubItemList
               PresentationContextItem {..}->
                 do put pcHeader
                    put pcIdentifier
                    put pcReserved1
                    put pcReserved2
                    put pcReserved3
                    mapM_ put pcItemList
               UnknownARQItem -> putWord8 0
               
               
  get = do arqItemHeader <- get
--           return $ ApplicationContextItem (ARQItemHeader 0x10 0 20) "name"
           case arqItemType arqItemHeader of
             0x10 -> do  
                        acName   <- replicateM (fromIntegral (arqItemLength arqItemHeader)) get
                        return $ ApplicationContextItem arqItemHeader acName                       
             0x20 -> populatePresentationContext arqItemHeader
             0x21 -> populatePresentationContext arqItemHeader
             0x50 -> do 
                        usrItemList <- replicateM (fromIntegral (arqItemLength arqItemHeader)) get
                        return $ UserInformationItem arqItemHeader usrItemList
             _    -> return UnknownARQItem
        where populatePresentationContext header  =
                do 
                   pcidentifier <- get
                   pcreserved1  <- get
                   pcreserved2  <- get
                   pcreserved3  <- get
                   pcitemlist   <- replicateM (fromIntegral (arqItemLength header)-4) get
                   return $ PresentationContextItem header pcidentifier pcreserved1 pcreserved2 pcreserved3 pcitemlist


unpackARQItemList::BL.ByteString -> [ARQItem]
unpackARQItemList bl
  | BL.null bl  =  []
  | otherwise   =  let item = decode bl
                   in item:unpackARQItemList (BL.drop (arqPackedItemLength item) bl)
{-
Calculate the total length of the item
Length in the header is the length of the payload
Need to add the size of the ARQ header
-}
arqPackedItemLength::ARQItem -> Int64
arqPackedItemLength item = fromIntegral $ arqItemLength (getARQHeader item) + 4 

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
            
