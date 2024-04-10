module Bench.Prime where

import Bench.Galois
import Control.Monad.Random
import Criterion.Main
import Data.Field.Galois
import Protolude

type Fq = Prime 21888242871839275222246405745257275088696311157297823662689037894645226208583

fq :: Fq
fq = evalRand getRandom $ mkStdGen 0

fq' :: Fq
fq' = evalRand getRandom $ mkStdGen 1

benchPrime :: Benchmark
benchPrime = benchmark "Prime" fq fq'
