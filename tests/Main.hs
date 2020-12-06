module Main where

import Test.Tasty

import qualified Models

main :: IO()
main = do
    tests <- sequence specs
    defaultMain $ testGroup "Tests" tests
  where
    specs = [
        Models.spec
        ]
