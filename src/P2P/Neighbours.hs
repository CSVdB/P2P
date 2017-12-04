module P2P.Neighbours where

import Import

import P2P.Chan
import P2P.JSONUtils
import P2P.SockAddr
import P2P.SocketInfo

import Test.QuickCheck.Gen

updateNeighbours :: AddrChan -> SockAddr -> IO ()
updateNeighbours chan addr = do
    addresses <- readEverything chan
    neighbours <- gen minNumOfNeighbours addresses
    notify neighbours addr

gen :: (Eq a) => Int -> [a] -> IO [a]
gen n xs =
    if length xs < n
        then pure xs
        else genMore n xs []

genMore :: (Eq a) => Int -> [a] -> [a] -> IO [a]
genMore 0 _ xs = pure xs
genMore n list xs = do
    x <- generate $ elements list
    if x `elem` xs
        then genMore n list xs
        else genMore (n - 1) list (x : xs)

notify :: [SockAddr] -> SockAddr -> IO ()
notify addresses addr = do
    mapM_ (sendInfo (encode addr)) addresses
    flip sendInfo addr $ encode addresses

sendInfo :: ByteString -> SockAddr -> IO ()
sendInfo msg addr = do
    sock <- getSocket
    connect sock addr
    sendAll sock msg

minNumOfNeighbours :: Int
minNumOfNeighbours = 5
