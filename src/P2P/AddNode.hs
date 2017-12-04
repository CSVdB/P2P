module P2P.AddNode where

import Control.Concurrent (forkIO)

import P2P.Chan
import P2P.SockAddr
import P2P.SocketInfo

addnode :: IO ()
addnode = do
    sock <- getBoundSocket
    addr <- getSocketName sock
    connectToNetwork addr
    addrChan <- newTChanIO
    listen sock 2
    listeningNode sock addrChan

listeningNode :: Socket -> AddrChan -> IO ()
listeningNode sock addrChan = do
    (conn, _) <- accept sock
    _ <- forkIO $ runConn conn addrChan
    listeningNode sock addrChan

-- Listen for messages from the server which should tell you
-- about new neighbours
-- as well as messages from new neighbours "ByteString" (holding a key value pair)
runConn :: Socket -> AddrChan -> IO ()
runConn _ _ = undefined

connectToNetwork :: SockAddr -> IO ()
connectToNetwork _ = undefined
-- Send a message to the server with your socket address
