{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module DroneDhall.Models where

import Data.Aeson
import GHC.Generics (Generic)

data Config = Config
  { data' :: String
  , kind :: String
  }
  deriving stock (Eq, Show)

data Repository = Repository
  { id :: Integer
  , trusted :: Bool
  , protected :: Bool
  }
  deriving stock (Eq, Show, Generic)

data Request = Request
  { config :: Config
  , repo :: Repository
  }
  deriving (Show)


instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "data"
    <*> v .:? "kind" .!= ""

instance FromJSON Repository

instance ToJSON Config where
  toJSON (Config data' _) = object ["data" .= data']
