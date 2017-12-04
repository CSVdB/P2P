module P2P.Neighbours where

import P2P.Chan
import P2P.SockAddr

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
notify = undefined

minNumOfNeighbours :: Int
minNumOfNeighbours = 5
