module P2P.StartServer where

import Network.Socket

import Control.Concurrent (forkIO)

import P2P.SocketInfo

startserver :: IO ()
startserver = do
    sock <- getServerSocket
    listen sock 2
    mainloop sock

mainloop :: Socket -> IO ()
mainloop sock = do
    (conn, _) <- accept sock
    _ <- forkIO $ runConn conn
    mainloop sock

runConn :: Socket -> IO ()
runConn _ = undefined
    -- Use connection with new node to get the socket address
    -- Add socket address to current list
    -- Choose new socket's neighbours randomly
    -- Notify everyone of their new neighbours
