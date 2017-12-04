module P2P
    ( p2p
    ) where

import P2P.AddNode
import P2P.OptParse
import P2P.StartServer

p2p :: IO ()
p2p = do
    instr <- getInstruction
    execute instr

execute :: Instruction -> IO ()
execute StartServer = startserver
execute AddNode = addnode
