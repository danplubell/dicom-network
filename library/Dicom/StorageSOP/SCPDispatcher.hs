module Dicom.StorageSOP.SCPDispatcher where

import Network.Socket
import qualified Network.BSD as BSD
import qualified Network.Socket.ByteString.Lazy as NSB
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Applicative
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Dicom.Network.Associate.Types
import Data.Int

maxConnToListenTo::Int
maxConnToListenTo = 5

maxReceiveBufferSize::Int64
maxReceiveBufferSize = 16384

defaultReceiveBufferSize::Int
defaultReceiveBufferSize = 65536


{-
The dispatcher setups the port on the specific host and starts a listening loop
When the dispatcher receives a message it spawns a new thread that activates the StorageClassSCP
-}
startDispatcher:: String->PortNumber->IO ()
startDispatcher host port = withSocketsDo $ do 
  protocol <- BSD.protoNumber <$> BSD.getProtocolByName "TCP"
  withSocket AF_INET Stream protocol $ \skt -> do
    targethost <- inet_addr host
    let sktAddr = SockAddrInet port targethost
    bind skt sktAddr
    listen skt maxConnToListenTo
    forever $ acceptConn skt
{-
Accepts a connection and forks a thread when a new connection is received
-}
acceptConn::Socket -> IO ()
acceptConn sock = do
     (conn,_) <- accept sock
     _<-forkIO $ runStorageClassSCP conn
     return ()
     
{-
Executes function with a specific socket
If an exception is thrown then the socket is shutdown
-}
withSocket :: Family -> SocketType -> ProtocolNumber -> (Socket -> IO a) -> IO a
withSocket family sktType protocol =
  bracket (socket family sktType protocol)
          (\skt -> putStrLn "Shutdown in withSocket" >> shutdown skt ShutdownBoth >> close skt)

withAccept :: Socket -> ((Socket, SockAddr) -> IO a) -> IO a
withAccept skt =
  bracket (accept skt)
          (\(connSkt, _ ) -> do
              putStrLn "Shutdown in withAccept" >> shutdown connSkt ShutdownBoth
              close connSkt)

{-
This is the primary storage class scp fuction.
The first message that is sent on a new connection should be an association request.
If the association request is received then is sent for further processing
If an abort PDU or the PDU type isn't recogized then the thread is killed and the process starts again
-}
runStorageClassSCP::Socket->IO ()
runStorageClassSCP skt = do
  --Accept first PDU.  
  pdu <- NSB.recv skt maxReceiveBufferSize
  
  case getPDUType pdu  of
    A_ABORT -> error "abort received" -- need to refactor this to handle exceptions
    A_ASSOCIATE_RQ -> processAssociateRQ ( unpackPDU pdu) skt
    _              -> error "unrecognized message" --refactor this to handle exceptions
    
  runStorageClassSCP skt
  return ()     

{-
Attempts to get the PDU type from a given bytestring.
The PDU type is the first byte int the bytestring
-}
getPDUType::BL.ByteString -> PDUType  
getPDUType  bs = if BL.null bs then UNKNOWN_PDU
                   else toEnum (fromIntegral $ head $ BL.unpack bs)::PDUType


processAssociateRQ:: AssociateRQPDU -> Socket-> IO ()
processAssociateRQ pdu skt = do
  putStrLn $ "processAssociateRQ: " ++ calledAETitle pdu    
  sendAssocAccept skt            --An associate request was received, now send back the association accept message
  forever $ processRequests skt  --Ready for processing of additional requests.  Continue to do this until release, abort, or exception
  
  
sendAssocAccept::Socket -> IO ()
sendAssocAccept skt = void $ NSB.send skt (packPDU buildAssociateACPDU)

{-
Possible Requests:
0x04 P-DATA-TF
0x05 A-RELEASE-RQ
0x07 A-ABORT
otherwise A-ABORT with ARTIM timer
-}
processRequests::Socket -> IO ()
processRequests skt =  do
  pdu <- NSB.recv skt maxReceiveBufferSize
  case getPDUType pdu of
    A_ABORT      -> do putStrLn "A_ABORT"
                       error "A_ABORT processing requests"
    A_RELEASE_RQ -> do putStrLn "A_RELEASE_RQ"
                       sendAssociateRelRP skt
                       error "A_RELEASE_RQ received and answered, end request processing"
    P_DATA_TF    -> putStrLn "P_DATA_TF" 
    UNKNOWN_PDU  -> do putStrLn "UNKNOWN_PDU"
                       print pdu
    _            -> putStrLn "Unexpected PDU"
  return ()


sendAssociateRelRP::Socket -> IO ()
sendAssociateRelRP skt = do void $ NSB.send skt (packPDU buildAssociateRelRP)
                            return ()
{-                       
waitForPDataPDUsUntilHandlerReportsDone
Association.WaitForPDataPDUs
-}
