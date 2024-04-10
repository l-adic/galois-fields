module Main where

import Bench.Binary
import Bench.Extension
import Bench.Prime
import Criterion.Main
import Protolude

main :: IO ()
main =
  defaultMain
    [benchBinary, benchExtension, benchPrime]
