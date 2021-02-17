{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Encoding                   (decodeUtf8)
import Dhall.Yaml                           (defaultOptions, dhallToYaml)
import DroneDhall.Models                    (Config (..), config)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty                           (ActionM, json, jsonData, liftAndCatchIO, middleware,
                                             post, scotty)

main :: IO ()
main = do
  scotty 3000 $ do
    middleware logStdoutDev

    post "/" $ do
      req <- jsonData

      let resp = Config . decodeUtf8 <$> converter (dhallSource req)
      result <- liftAndCatchIO resp

      json result
      where
        dhallSource req = data' . config $ req
        converter = dhallToYaml defaultOptions Nothing
