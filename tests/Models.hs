{-# LANGUAGE OverloadedStrings #-}

module Models
    ( spec
    ) where

import Data.Aeson        (decode, encode)
import DroneDhall.Models (Config (..), Repository (..))
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
                obj          = Config "some data" ""
                jsonText     = encode obj
                expectedText = "{\"data\":\"some data\"}"
            in jsonText `shouldBe` expectedText

    describe "repository" $
        it "should be parsable from json" $ do
            let jsonText = "{\"id\": 0, \"trusted\": false, \"protected\": false}"
                json     = decode jsonText :: Maybe Repository

            trusted <$> json `shouldBe` Just False
