{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Dicom.Network.TestHarness.TestAssociationSCU where

import Network
import Network.Socket hiding (send,sendTo,recv,recvFrom,accept)
import Dicom.Network.Associate.Types
import qualified Network.Socket.ByteString as NSB
import Control.Monad

---------Test Client
client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
  print addrInfo
  let serverAddr = head addrInfo
  print serverAddr
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  setSocketOption sock KeepAlive 1
  connect sock (addrAddress serverAddr)
  msgSender sock
  close sock


msgSender :: Socket -> IO ()
msgSender sock = do
    msg <- getLine
    case msg of
      "q"     -> close sock >> error "Time to quit"
      "abort" -> sendAbortPDU sock
      "assoc" -> do void (sendAssociatePDU sock)
                    pdu <- NSB.recv sock 1024
                    putStrLn "Received AssociateAC"
                    print pdu
      "release" -> do void (sendReleaseRQ sock)
                      pdu <- NSB.recv sock 1024
                      putStrLn "Receved ReleaseRP"
                      print pdu
                      
      _       -> putStrLn "Invalid option"
    msgSender sock
--    rMsg <- hGetLine sock
--    putStrLn rMsg
                    
sendAbortPDU::Socket-> IO()
sendAbortPDU sock = do NSB.send sock $ packPDU $ buildAbortPDU AbortServiceUser AbortUnspecified
                       return ()

sendAssociatePDU::Socket -> IO ()
sendAssociatePDU sock = do NSB.send sock $ packPDU $ buildAssociateRQPDU "CalledAETitle" "CallingAETitle"
                           return ()
sendReleaseRQ::Socket -> IO ()
sendReleaseRQ skt = do NSB.send skt $ packPDU buildAssociateRelRQ 
                       return ()
