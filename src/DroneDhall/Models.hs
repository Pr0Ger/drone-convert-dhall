{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


module DroneDhall.Models
  ( Config (..)
  , Repository (..)
  , Request (..)
  , config
  , config_path
  , data'
  , repo
  ) where

import Data.Aeson
import Data.Text     (Text)
import Lens.Micro.TH (makeLenses)

newtype Config = Config { _data' :: Text }
  deriving stock (Eq, Show)

newtype Repository = Repository { _config_path :: Text }
  deriving stock (Eq, Show)

data Request = Request
  { _config :: Config
  , _repo :: Repository
  }
  deriving stock (Eq, Show)

concat <$> mapM makeLenses [''Config, ''Repository, ''Request]

instance FromJSON Config where
  parseJSON (Object v) = Config
    <$> v .: "data"

instance ToJSON Config where
  toJSON (Config data') = object
    [ "data" .= data'
    ]

instance FromJSON Repository where
  parseJSON (Object v) = Repository
    <$> v .: "config_path"

instance FromJSON Request where
  parseJSON (Object v) = Request
    <$> v .: "config"
    <*> v .: "repo"
