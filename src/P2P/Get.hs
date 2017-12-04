module P2P.Get where

import qualified Data.HashMap as HM

import P2P.Chan
import P2P.KeyValPair

get :: Key -> StoreChan -> IO ()
get k store = do
    pairs <- readEverything store
    case HM.lookup k . HM.fromList $ toPair <$> pairs of
        Nothing ->
            putStrLn $ unwords ["There is no value for the key", k, "yet."]
        Just v -> putStrLn $ unwords ["The value for key", k, "is", v]
