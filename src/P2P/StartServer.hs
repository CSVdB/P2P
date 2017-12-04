module P2P.StartServer where

import Control.Concurrent (forkIO)

import P2P.Chan
import P2P.JSONUtils
import P2P.Neighbours
import P2P.SockAddr
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
runConn conn addrChan = do
    msg <- recv conn 1000
    case decode msg of
        Nothing -> putStrLn "Cannot parse message as a socket address"
        Just addr -> do
            updateNeighbours addrChan addr
            write addrChan addr
