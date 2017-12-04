{-# LANGUAGE ScopedTypeVariables #-}

module P2P.AddNode where

import Import

import Control.Concurrent (forkIO)

import P2P.Chan
import P2P.Get
import P2P.JSONUtils
import P2P.KeyValPair
import P2P.Put
import P2P.SockAddr
import P2P.SocketInfo

addnode :: IO ()
addnode = do
    sock <- getBoundSocket
    addr <- getSocketName sock
    connectToNetwork addr
    addrChan <- newTChanIO
    cleanChannel addrChan
    store <- newTChanIO
    cleanChannel store
    listen sock 2
    listeningNode sock addrChan store

connectToNetwork :: SockAddr -> IO ()
connectToNetwork addr = do
    sock <- getSocketConnectedToServer
    sendAll sock $ encode addr

listeningNode :: Socket -> AddrChan -> StoreChan -> IO ()
listeningNode sock addrChan store = do
    (conn, _) <- accept sock
    _ <- forkIO $ runConn conn addrChan store
    listeningNode sock addrChan store

-- Be able to receive bytestrings from neighbours,
-- holding a key value pair
runConn :: Socket -> AddrChan -> StoreChan -> IO ()
runConn conn addrChan store = do
    msg <- recv conn 1000
    decodeMsg msg addrChan store

decodeMsg :: ByteString -> AddrChan -> StoreChan -> IO ()
decodeMsg msg addrChan store = do
    isAddr <- decodeMsgAsAddr msg addrChan
    isAddrs <- decodeMsgAsAddrs msg addrChan
    isKeyVal <- decodeMsgAsKeyVal msg addrChan store
    when (isAddr || isAddrs || isKeyVal) $
        decodeMsgAsTerminalInput msg addrChan store

decodeMsgAsAddr :: ByteString -> AddrChan -> IO Bool
decodeMsgAsAddr msg addrChan =
    case decode msg of
        Just addr -> const True <$> write addrChan addr
        Nothing -> pure False

decodeMsgAsAddrs :: ByteString -> AddrChan -> IO Bool
decodeMsgAsAddrs msg addrChan =
    case decode msg of
        (Just addresses :: Maybe [SockAddr]) ->
            const True <$> mapM_ (write addrChan) addresses
        Nothing -> pure False

decodeMsgAsKeyVal :: ByteString -> AddrChan -> StoreChan -> IO Bool
decodeMsgAsKeyVal msg addrChan store =
    case decode msg of
        Just keyValPair -> const True <$> put keyValPair addrChan store
        Nothing -> pure False

decodeMsgAsTerminalInput :: ByteString -> AddrChan -> StoreChan -> IO ()
decodeMsgAsTerminalInput msg addrChan store = do
    let arguments = words $ show msg
    case arguments of
        "put":k:[v] ->
            let pair = KeyValPair k v
            in put pair addrChan store
        "get":[k] -> get k store
        _ -> putStrLn $ "Could not parse " ++ show msg
