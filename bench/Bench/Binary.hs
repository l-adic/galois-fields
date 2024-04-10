module Bench.Binary where

import Bench.Galois
import Control.Monad.Random
import Criterion.Main
import Data.Field.Galois
import Protolude

type F2m = Binary 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425

f2m :: F2m
f2m = evalRand getRandom $ mkStdGen 0

f2m' :: F2m
f2m' = evalRand getRandom $ mkStdGen 1

benchBinary :: Benchmark
benchBinary = benchmark "Binary" f2m f2m'
