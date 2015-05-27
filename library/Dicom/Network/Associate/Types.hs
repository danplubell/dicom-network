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

{-
Defines a class of structures that are used to exchange information
-}
class StructType a  where
  toStructType   :: Word8 -> a
  fromStructType :: a -> Word8  

getPDUTypeVal::PDUTypes -> Word8
getPDUTypeVal   = fromStructType 

{-
Attempts to get the PDU type from a given bytestring.
The PDU type is the first byte int the bytestring
-}
getPDUType::BL.ByteString -> PDUTypes  
getPDUType  bs = if BL.null bs then UNKNOWN_PDU
                   else toStructType (head $ BL.unpack bs)

calPDULength :: (Binary a, Num b) => a -> b
calPDULength pdu = fromIntegral $ BL.length (encode pdu)-4 

data PDUTypes = A_ASSOCIATE_RQ|A_ASSOCIATE_AC|A_ASSOCIATE_RJ|P_DATA_TF|A_RELEASE_RQ|A_RELEASE_RP|A_ABORT|UNKNOWN_PDU deriving (Show,Eq,Ord)
instance Binary PDUTypes where
  put a = put (fromStructType a)
  get = do t <- get
           return $ toStructType t


instance StructType PDUTypes where
  toStructType 1 = A_ASSOCIATE_RQ
  toStructType 2 = A_ASSOCIATE_AC
  toStructType 3 = A_ASSOCIATE_RJ
  toStructType 4 = P_DATA_TF
  toStructType 5 = A_RELEASE_RQ
  toStructType 6 = A_RELEASE_RP
  toStructType 7 = A_ABORT
  toStructType 0 = UNKNOWN_PDU
  toStructType _ = UNKNOWN_PDU
  
  fromStructType UNKNOWN_PDU    = 0
  fromStructType A_ASSOCIATE_RQ = 1
  fromStructType A_ASSOCIATE_AC = 2
  fromStructType A_ASSOCIATE_RJ = 3
  fromStructType P_DATA_TF      = 4
  fromStructType A_RELEASE_RQ   = 5
  fromStructType A_RELEASE_RP   = 6
  fromStructType A_ABORT        = 7
  
  
data PDUHeader =PDUHeader{ pduType::PDUTypes, pduHeaderReserved::Word8, pduLength::Word16} deriving (Show,Generic,Eq)

instance Binary PDUHeader

newAssociateRQPDU::AssociateRQPDU
newAssociateRQPDU = AssociateRQPDU {
    arqPDUHeader       = PDUHeader A_ASSOCIATE_RQ 0 0
  , arqReserved        = 0
  , arqProtocolVersion = 0
  , calledAETitle      = replicate 16 ' '
  , callingAETitle     = replicate 16 ' '
  , arqVariableItems   = []                 
  , arqReserved2       = replicate 32 0}

data AssociateRQPDU = AssociateRQPDU {
    arqPDUHeader       :: PDUHeader
  , arqProtocolVersion :: Word16
  , arqReserved        :: Word16
  , calledAETitle      :: String
  , callingAETitle     :: String
  , arqReserved2       :: [Word8] -- fixed length of 32 reserved bytes
  , arqVariableItems   :: [ARQItem] --includes Application Context Item, User Information Item, and Presentation Context.  These items can be in any order.  
  } deriving (Show,Generic,Eq)


instance Binary AssociateRQPDU where
  put a = do put (arqPDUHeader a)
             put (arqProtocolVersion a)
             put (arqReserved a)
             mapM_ put (take 16 $ calledAETitle a)
             mapM_ put (take 16 $ callingAETitle a)
             mapM_ put (arqReserved2 a)
             mapM_ put (arqVariableItems a)
  get = do arqHeader      <- get
           arqprotocolv   <- get
           arqreserved    <- get
           calledaetitle  <- replicateM 16 get
           callingaetitle <- replicateM 16 get
           arqreserved2   <- replicateM 32 get
           arqItemBytes   <- replicateM (fromIntegral (pduLength arqHeader)-68) get            
           return $ AssociateRQPDU arqHeader arqprotocolv arqreserved calledaetitle callingaetitle arqreserved2
                                   (unpackARQItemList (BL.pack arqItemBytes))



instance PDU AssociateRQPDU where
  packPDU pdu = encode $ pdu { arqPDUHeader = (arqPDUHeader pdu)
                               {pduLength = calPDULength pdu}}
 

type CalledAETitle = String
type CallingAETitle = String
type ImplementationClassUID = String
type PDUTypeVal = Int

buildAssociateRQPDU::CalledAETitle ->CallingAETitle ->AssociateRQPDU
buildAssociateRQPDU toAE fromAE=
  AssociateRQPDU (PDUHeader A_ASSOCIATE_RQ 0 0)
                   0
                   0
                   toAE
                   fromAE
                   (replicate 32 0)
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
buildAssociateACPDU = AssociateACPDU (PDUHeader A_ASSOCIATE_AC 0 0)
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
buildAssociateRelRQ = AssociateRelRQPDU (PDUHeader A_RELEASE_RQ 0 0) 0

data AssociateRelRPPDU = AssociateRelRPPDU {
    arrpPDUHeader :: PDUHeader
  , arrpReserved  :: Word32
  } deriving (Show,Generic)
             
instance Binary AssociateRelRPPDU


instance PDU AssociateRelRPPDU where
  packPDU pdu = encode $ pdu {arrpPDUHeader = (arrpPDUHeader pdu)
                              {pduLength = calPDULength pdu}}


buildAssociateRelRP::AssociateRelRPPDU
buildAssociateRelRP = AssociateRelRPPDU (PDUHeader A_RELEASE_RP 0 0) 0


{-
0 - reason-not-specified1 - unrecognized-PDU
2 - unexpected-PDU
3 - reserved
4 - unrecognized-PDU parameter
5 - unexpected-PDU parameter
6 - invalid-PDU-parameter value
-}


data AbortSource = AbortServiceProvider|AbortServiceUser|InvalidAbortSource
instance StructType AbortSource where
  fromStructType AbortServiceProvider = 2
  fromStructType AbortServiceUser     = 0
  fromStructType InvalidAbortSource   = 255

  toStructType 2 = AbortServiceProvider
  toStructType 0 = AbortServiceUser
  toStructType _ = InvalidAbortSource 

data AbortReason = AbortUnspecified | UnexpectedPDU | UnrecogPDUParam | UnexpectedPDUParam|InvalidPDUParamVal 
instance StructType AbortReason where
  fromStructType AbortUnspecified   = 0
  fromStructType UnexpectedPDU      = 2
  fromStructType UnrecogPDUParam    = 4
  fromStructType UnexpectedPDUParam = 5
  fromStructType InvalidPDUParamVal = 6

  toStructType 0 = AbortUnspecified
  toStructType 2 = UnexpectedPDU
  toStructType 4 = UnrecogPDUParam
  toStructType 5 = UnexpectedPDUParam
  toStructType 6 = InvalidPDUParamVal
  toStructType _ = AbortUnspecified  

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
buildAbortPDU s r = AbortPDU (PDUHeader A_ABORT 0 4)
                             0
                             (fromStructType r)
                             (fromStructType s)


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

data ARQItemType = ApplicationContextItemT | UserInformationItemT | PresentationContextItemRQT|PresentationContextItemACT|UnknownARQItemT deriving (Show,Eq)

instance Binary ARQItemType  where
  put a = put (fromStructType a)
  get = do t <- get
           return $ toStructType t
           
instance StructType ARQItemType where
  fromStructType ApplicationContextItemT    = 0x10
  fromStructType UserInformationItemT       = 0x50
  fromStructType PresentationContextItemRQT = 0x20
  fromStructType PresentationContextItemACT = 0x21
  fromStructType _                          = 0

  toStructType 0x10 = ApplicationContextItemT
  toStructType 0x50 = UserInformationItemT
  toStructType 0x20 = PresentationContextItemRQT
  toStructType 0x21 = PresentationContextItemACT
  toStructType _    = UnknownARQItemT
  
data ARQItemHeader = ARQItemHeader
                     {
                        arqItemType       ::ARQItemType
                     ,  arqItemReserved   ::Word8
                     ,  arqItemLength     ::Word16
                     } deriving (Show, Eq, Generic)
instance Binary ARQItemHeader

class ARQItemT a where
  getARQHeader::a -> ARQItemHeader

data ARQItem = ApplicationContextItem
               {
                 acnHeader      ::ARQItemHeader
               , acnContextName ::String
               }
             | UserInformationItem
               {
                 uiiHeader      ::ARQItemHeader
               , uiiSubItemList ::[UserInformationSubItem]
               }
             | PresentationContextItem
               {
                 pcHeader            ::ARQItemHeader
               , pcIdentifier        ::Word8
               , pcReserved1         ::Word8
               , pcReserved2         ::Word8
               , pcReserved3         ::Word8
               , pcItemList          ::[PresentationContextSubItem]
               }
             | UnknownARQItem        
             deriving (Show,Generic,Eq)

instance ARQItemT ARQItem where
  getARQHeader ApplicationContextItem{..}  = acnHeader 
  getARQHeader UserInformationItem{..}     = uiiHeader
  getARQHeader PresentationContextItem{..} = pcHeader
  getARQHeader UnknownARQItem              = ARQItemHeader (toStructType 0) 0 0
  
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
           case arqItemType arqItemHeader of
             ApplicationContextItemT    -> do  
                                            acName   <- replicateM (fromIntegral (arqItemLength arqItemHeader)) get
                                            return $ ApplicationContextItem arqItemHeader acName                       
             PresentationContextItemRQT -> populatePresentationContext arqItemHeader
             PresentationContextItemACT -> populatePresentationContext arqItemHeader
             UserInformationItemT       -> do 
                                            usrItemList <- replicateM (fromIntegral (arqItemLength arqItemHeader)) get
                                            return $ UserInformationItem arqItemHeader (unpackUserSubItemList (BL.pack usrItemList))
             _                          -> return UnknownARQItem
        where populatePresentationContext header  =
                do 
                   pcidentifier <- get
                   pcreserved1  <- get
                   pcreserved2  <- get
                   pcreserved3  <- get
                   pcitemlist   <- replicateM (fromIntegral (arqItemLength header)-4) get -- subtract 4 for the identifier and reserved fields
                   return $ PresentationContextItem header pcidentifier pcreserved1 pcreserved2 pcreserved3 (unpackPCSubItemList (BL.pack pcitemlist))


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

data SubItemTypes = AbstractSyntaxT | TransferSyntaxT|ImplementationClassUIDT | ImplementationVersionNameT
                                     | MaximumLengthReceivedT | UnknownSubItemT deriving (Show, Eq)
instance Binary SubItemTypes where
  put a = put (fromStructType a)
  get = do t <- get
           return $ toStructType t
           
instance StructType SubItemTypes where
  fromStructType AbstractSyntaxT            = 0x30
  fromStructType TransferSyntaxT            = 0x40
  fromStructType ImplementationClassUIDT    = 0x52
  fromStructType ImplementationVersionNameT = 0x55
  fromStructType MaximumLengthReceivedT     = 0x51
  fromStructType _                          = 0

  toStructType 0x30 = AbstractSyntaxT
  toStructType 0x40 = TransferSyntaxT
  toStructType 0x52 = ImplementationClassUIDT
  toStructType 0x55 = ImplementationVersionNameT
  toStructType 0x51 = MaximumLengthReceivedT
  toStructType _    = UnknownSubItemT
  
  

data SubItemHeader = SubItemHeader { itemType::SubItemTypes, itemReserved::Word8, itemLength::Word16}
  deriving (Show,Generic,Eq)

instance Binary SubItemHeader

unpackPCSubItemList::BL.ByteString -> [PresentationContextSubItem]
unpackPCSubItemList bl
     | BL.null bl = []
     | otherwise  = let item = decode bl
                    in item:unpackPCSubItemList (BL.drop (pcSubItemPackedLength item) bl )
                       
pcSubItemPackedLength::PresentationContextSubItem -> Int64
pcSubItemPackedLength item = fromIntegral $ itemLength (getSubItemHeader item) + 4 -- add the size of sub item header to get total length

unpackUserSubItemList::BL.ByteString -> [UserInformationSubItem]
unpackUserSubItemList bl
   | BL.null bl = []
   | otherwise  = let item = decode bl
                  in item:unpackUserSubItemList (BL.drop (userSubItemPackedLength item) bl)
                     
userSubItemPackedLength::UserInformationSubItem -> Int64
userSubItemPackedLength item = fromIntegral $ itemLength (getSubItemHeader item) + 4


class SubItemType a where
  getSubItemHeader::a-> SubItemHeader

data PresentationContextSubItem =
                AbstractSyntax{
                   asItemHeader  ::SubItemHeader
                 , asName        ::String}
              | TransferSyntax{
                   tsItemHeader ::SubItemHeader
                 , tsName       ::String}
              | UnknownPCItem
              deriving (Show,Generic,Eq)

instance SubItemType PresentationContextSubItem where
  getSubItemHeader AbstractSyntax{..} = asItemHeader
  getSubItemHeader TransferSyntax{..} = tsItemHeader
  getSubItemHeader UnknownPCItem = SubItemHeader UnknownSubItemT 0 0

  
instance Binary PresentationContextSubItem where
  put i = case i of
            AbstractSyntax {..} -> do put asItemHeader
                                      mapM_ put asName
                               
            TransferSyntax {..} -> do put tsItemHeader
                                      mapM_ put tsName
            UnknownPCItem       -> putWord8 0
  get = do
    itemHeader <- get
    case itemType itemHeader of
      TransferSyntaxT -> do 
                 tsname       <- replicateM (fromIntegral $ itemLength itemHeader) get
                 return $ TransferSyntax itemHeader tsname
      AbstractSyntaxT -> do 
                 asname       <- replicateM (fromIntegral $ itemLength itemHeader) get
                 return $ AbstractSyntax itemHeader asname 
      _    -> return UnknownPCItem

data UserInformationSubItem = ImplementationClassUID        {   icItemHeader ::SubItemHeader
                                                              , icUID        ::String}
                            | ImplementationVersionName     {   ivnItemHeader::SubItemHeader
                                                              , ivn          ::String}
                            | MaximumLengthReceived         {   mlItemHeader ::SubItemHeader
                                                              , mlMaxLength  ::Word32}
                            | UnknownSubItem
                                                            deriving (Show,Generic, Eq)
instance SubItemType UserInformationSubItem where
  getSubItemHeader ImplementationClassUID {..}    = icItemHeader
  getSubItemHeader ImplementationVersionName {..} = ivnItemHeader
  getSubItemHeader MaximumLengthReceived{..}      = mlItemHeader
  getSubItemHeader UnknownSubItem                 = SubItemHeader UnknownSubItemT 0 0
  
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
            MaximumLengthReceivedT -> do
                      mlmaxlength <- get
                      return $ MaximumLengthReceived header mlmaxlength
            ImplementationClassUIDT -> do
                      icUID <- replicateM (fromIntegral $ itemLength header) get
                      return $ ImplementationClassUID header icUID
            ImplementationVersionNameT -> do
                      ivn <- replicateM (fromIntegral $ itemLength header) get
                      return $ ImplementationVersionName header ivn
            _    -> return UnknownSubItem          
            

{-
Replace a value at a position in a bytestring. 
-}
replaceValueAt::(Binary a) => Int64 -> a -> BL.ByteString -> BL.ByteString
replaceValueAt p v s = let t = BL.splitAt p s
                       in splice (fst t) ( encode v) (snd t)
                       where splice b c d = BL.concat [ b , c , BL.drop (BL.length c) d]
                             
