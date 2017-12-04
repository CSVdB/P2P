module P2P.SocketInfo where

import Network.Socket

serverAddr :: SockAddr
serverAddr = SockAddrInet 4242 hostAddr

hostAddr :: HostAddress
hostAddr = iNADDR_ANY

getServerSocket :: IO Socket
getServerSocket = do
    sock <- getSocket
    bind sock serverAddr
    pure sock

getSocket :: IO Socket
getSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    pure sock

getBoundSocket :: IO Socket
getBoundSocket = do
    sock <- getSocket
    bind sock $ SockAddrInet 0 hostAddr
    pure sock
