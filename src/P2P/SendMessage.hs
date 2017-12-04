module P2P.SendMessage where

import Import

import Data.Aeson hiding (encode)

import P2P.JSONUtils
import P2P.SockAddr
import P2P.SocketInfo

sendKeyValuePair :: (ToJSON k, ToJSON v) => k -> v -> SockAddr -> IO ()
sendKeyValuePair key value addr =
    let msg = encode (key, value)
    in sendMessage msg addr

sendMessage :: ByteString -> SockAddr -> IO ()
sendMessage msg addr = do
    sock <- getSocket
    connect sock addr
    sendAll sock msg
