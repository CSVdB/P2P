module P2P.Get where

import qualified Data.HashMap as HM

import P2P.Chan
import P2P.KeyValPair

get :: Key -> StoreChan -> IO (Maybe Value)
get k store = do
    pairs <- readEverything store
    pure . HM.lookup k . HM.fromList $ toPair <$> pairs
