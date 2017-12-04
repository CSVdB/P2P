{-# LANGUAGE ScopedTypeVariables #-}

module P2P.AddNode where

import Import

import Control.Concurrent (forkIO)

import P2P.Chan
import P2P.JSONUtils
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

-- Be able to receive bytestrings from neighbours,
-- holding a key value pair
runConn :: Socket -> AddrChan -> IO ()
runConn conn addrChan = do
    msg <- recv conn 1000
    decodeMsgAsSockAddr msg addrChan

decodeMsgAsSockAddr :: ByteString -> AddrChan -> IO ()
decodeMsgAsSockAddr msg addrChan =
    case decode msg of
        Just addr -> write addrChan addr
        Nothing ->
            case decode msg of
                (Just addresses :: Maybe [SockAddr]) ->
                    mapM_ (write addrChan) addresses
                Nothing -> putStrLn "Cannot parse as socket addresses"

connectToNetwork :: SockAddr -> IO ()
connectToNetwork addr = do
    sock <- getSocketConnectedToServer
    sendAll sock $ encode addr
