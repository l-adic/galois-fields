module Main where

import Protolude
import Test.Binary
import Test.Extension
import Test.Prime
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup "Tests" [testPrime, testExtension, testBinary]
