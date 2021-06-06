{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString                      (ByteString)
import Data.Text                            (Text, unpack)
import Data.Text.Encoding                   (decodeUtf8, encodeUtf8)
import Dhall.Yaml                           (Options (documents), defaultOptions, dhallToYaml)
import DroneDhall.Models                    (Config (Config), config, config_path, data', repo)
import Lens.Micro.Platform                  ((^.))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.FilePath                      (takeExtension)
import Web.Scotty                           (ActionM, json, jsonData, liftAndCatchIO, middleware, post, scotty)
dhallOptions :: Options
dhallOptions = defaultOptions { documents  = True }

processPipilineConfig :: String -> Text -> IO ByteString
processPipilineConfig ".dhall" config = dhallToYaml dhallOptions Nothing config
processPipilineConfig _ config        = return $ encodeUtf8 config

main :: IO ()
main = do
  scotty 3000 $ do
    middleware logStdoutDev

    post "/" $ do
      req <- jsonData

      let resp = Config . decodeUtf8 <$> processPipilineConfig (extension req) (configSource req)
      result <- liftAndCatchIO resp

      json result
      where
        extension req = takeExtension $ unpack $ req^.repo.config_path
        configSource req = req^.config.data'
