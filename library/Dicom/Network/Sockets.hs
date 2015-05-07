module Sockets where

import Network.Socket
import Control.Exception

withSocket::Family -> SocketType -> ProtocolNumber -> (Socket -> IO a) -> IO a
withSocket family sktType protocol =
  bracket (socket family sktType protocol)
          (\s -> shutdown s ShutdownBoth >> close s)

withAccept :: Socket -> ((Socket, SockAddr) -> IO a) -> IO a
withAccept skt = bracket (accept skt)
                         (\(connSkt, _ ) -> do
                             shutdown connSkt ShutdownBoth
                             close connSkt)


