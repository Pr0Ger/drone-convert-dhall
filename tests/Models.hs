{-# LANGUAGE OverloadedStrings #-}

module Models
  ( spec
  ) where

import Data.Aeson        (decode, encode)
import DroneDhall.Models (Config (..), Request (..))
import Test.Tasty        (TestTree)
import Test.Tasty.Hspec  (describe, it, shouldBe, testSpec)

spec :: IO TestTree
spec = testSpec "json models" $ do
  describe "config" $ do
    it "should be parsable from json" $
      let
        jsonText = "{\"data\": \"some data\"}"
        json     = decode jsonText :: Maybe Config
      in data' <$> json `shouldBe` Just "some data"

    it "should be encodable to json" $
      let
        obj          = Config "some data"
        jsonText     = encode obj
        expectedText = "{\"data\":\"some data\"}"
      in jsonText `shouldBe` expectedText

  describe "request" $ do
    it "should be parsable from json" $
      let
        jsonText = "{\"config\": {\"data\": \"test config file\"}}"
        json     = decode jsonText :: Maybe Request
      in data' . config <$> json `shouldBe` Just "test config file"
