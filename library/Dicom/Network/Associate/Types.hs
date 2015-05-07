{-# LANGUAGE DeriveGeneric #-}
module Dicom.Network.Associate.Types where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Word
import GHC.Generics
import Data.Binary

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
  , arqVariableItems      = [[]]                 
  , arqReserved2       = BL.replicate 32 0}

data AssociateRQPDU = AssociateRQPDU {
    arqPDUHeader       :: PDUHeader
  , arqProtocolVersion :: Word16
  , arqReserved        :: Word16
  , calledAETitle      :: String
  , callingAETitle     :: String
  , arqReserved2       :: BL.ByteString -- fixed length of 32 reserved bytes
  , arqVariableItems      :: [[Word8]] --includes Application Context Item, User Information Item, and Presentation Context.  These items can be in any order.  
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
                   [[]]



    
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




data ItemHeader = ItemHeader { itemType::Word8, itemReserved::Word8, itemLength::Word16}
  deriving (Show,Generic)
instance Binary ItemHeader

data ApplicationContextItem = ApplicationContextItem {
    acnType        ::Int
  , acnLength      ::Int
  , acnContextName ::String
  } deriving (Show,Eq,Read,Generic)
instance Binary ApplicationContextItem
                              
data UserInformationItem = UserInformationItem {
   uiiType         ::Int
  , uiiLength      ::Int
  , uiiSubItemList ::[UserInformationSubItem]
  } deriving (Show,Generic)
instance Binary UserInformationItem

data SCPSCURoleSelection = SCPRoleSelection {
    scuRole :: Bool
  , scpRole :: Bool
  , abstractSyntaxUID::String
  } deriving (Show,Generic)
instance Binary SCPSCURoleSelection

data UserInformationSubItem = ImplementationClassUID        { icUID                ::String}
                            | SCUSCPRoleSelection           { roleSelection        ::SCPSCURoleSelection}
                            | ImplementationVersionName     { ivn                  ::String}
                            | MaximumLengthReceived         { maxLenR              ::Int}
                            | SOPClassExtendedNegotiation   {
                                                              sopClassUIDLength    ::Int
                                                            , sopClassUID          ::String
                                                            , info                 ::BL.ByteString
                                                            }
                            | UserIdentityNegotationAccept  {
                                                              serverResponseLength ::Int
                                                            , serverResponse       ::BL.ByteString
                                                            }
                            | UserIdentityNegotiationRequest{
                                                              userIdentityType     ::Int
                                                            , posRespRequired      ::Int
                                                            , primaryFieldLen      ::Int
                                                            , primaryField         ::BL.ByteString
                                                            , secondaryFieldLen    ::Int
                                                            , secondaryField       ::BL.ByteString
                                                            } deriving (Show,Generic)
instance Binary UserInformationSubItem

data PresentationContext = PresentationContext {
    pcIdentifier        ::Char
  , pcResultReason      ::Char
  , pcAbstractSyntaxUID ::String
  , transferSyntacUIDs  ::[String]
  } deriving (Show,Generic)

instance Binary PresentationContext

