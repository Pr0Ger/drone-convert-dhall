{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

module DroneDhall.Models
  ( Config (..)
  , Request (..)
  ) where

import Data.Aeson
import Data.Text    (Text)
import GHC.Generics (Generic)

newtype Config = Config { data' :: Text }
  deriving stock (Eq, Show)

newtype Request = Request { config :: Config }
  deriving stock (Generic, Show)

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "data"

instance ToJSON Config where
  toJSON (Config data') = object ["data" .= data']

instance FromJSON Request
