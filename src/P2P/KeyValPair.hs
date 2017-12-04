{-# LANGUAGE DeriveGeneric #-}

module P2P.KeyValPair where

import Import

import Data.Aeson

data KeyValPair = KeyValPair
    { key :: String
    , val :: String
    } deriving (Eq, Show, Generic)

instance FromJSON KeyValPair

instance ToJSON KeyValPair
