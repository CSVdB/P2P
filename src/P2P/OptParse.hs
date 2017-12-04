module P2P.OptParse where

import Import

import System.Environment

getInstruction :: IO Instruction
getInstruction = do
    args <- getArgs
    case parseArgs args of
        Just x -> pure x
        Nothing -> die "Cannot parse the arguments"

data Instruction
    = StartServer
    | AddNode
    deriving (Eq, Show)

parseArgs :: [String] -> Maybe Instruction
parseArgs xs =
    case xs of
        ["startserver"] -> Just StartServer
        ["addnode"] -> Just AddNode
        _ -> Nothing
