module Main where

import Test.Tasty
import Test.Tasty.Hspec

main :: IO()
main = do
    hspecTree <- spec
    defaultMain $ testGroup "Tests"
        [
            hspecTree
        ]

spec :: IO TestTree
spec = testSpec "hspec" $ describe "test" $
    it "5 == 5" $
        5 `shouldBe` (5 :: Int)
