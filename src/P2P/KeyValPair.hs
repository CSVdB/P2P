{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module P2P.KeyValPair where

import Import

import Data.Aeson (FromJSON, ToJSON)

data KeyValPair = KeyValPair
    { key :: Key
    , val :: Value
    } deriving (Eq, Show, Generic)

toPair :: KeyValPair -> (Key, Value)
toPair KeyValPair {..} = (key, val)

type Key = String

type Value = String

instance FromJSON KeyValPair

instance ToJSON KeyValPair
