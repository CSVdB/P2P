module P2P.StartServer where

import Network.Socket

import Control.Concurrent (forkIO)

import P2P.Chan
import P2P.SocketInfo

startserver :: IO ()
startserver = do
    sock <- getServerSocket
    addrChan <- newTChanIO
    listen sock 2
    mainloop sock addrChan

mainloop :: Socket -> AddrChan -> IO ()
mainloop sock addrChan = do
    (conn, _) <- accept sock
    _ <- forkIO $ runConn conn addrChan
    mainloop sock addrChan

runConn :: Socket -> AddrChan -> IO ()
runConn _ _ = undefined
    -- Use connection with new node to get the socket address
    -- Add socket address to current list
    -- Choose new socket's neighbours randomly
    -- Notify everyone of their new neighbours
