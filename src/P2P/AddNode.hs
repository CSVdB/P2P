module P2P.AddNode where

import Network.Socket

import Control.Concurrent (forkIO)

import P2P.SocketInfo

addnode :: IO ()
addnode = do
    sock <- getBoundSocket
    addr <- getSocketName sock
    connectToNetwork addr
    listen sock 2
    listeningNode sock

listeningNode :: Socket -> IO ()
listeningNode sock = do
    (conn, _) <- accept sock
    _ <- forkIO $ runConn conn
    listeningNode sock

-- Listen for messages from the server which should tell you
-- about new neighbours
-- as well as messages from new neighbours "ByteString" (holding a key value pair)
runConn :: Socket -> IO ()
runConn _ = undefined

connectToNetwork :: SockAddr -> IO ()
connectToNetwork _ = undefined
-- Send a message to the server with your socket address
