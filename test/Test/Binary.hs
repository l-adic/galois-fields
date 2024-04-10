module Test.Binary where

import Data.Field.Galois
import Protolude
import Test.Galois
import Test.Tasty

type F2A = Binary 0x20000000000000000000000000201

type F2B = Binary 0x80000000000000000000000000000010d

type F2C = Binary 0x800000000000000000000000000000000000000c9

type F2D = Binary 0x2000000000000000000000000000000000000000000008001

type F2E = Binary 0x20000000000000000000000000000000000000004000000000000000001

type F2F = Binary 0x800000000000000000004000000000000000000000000000000000000001

type F2G = Binary 0x800000000000000000000000000000000000000000000000000000000000000000010a1

type F2H = Binary 0x2000000000000000000000000000000000000000000000000000000000000000000000000000000008000000000000000000001

type F2I = Binary 0x80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000425

testBinary :: TestTree
testBinary =
  testGroup
    "Binary fields"
    [ test "F2A" (witness :: F2A),
      test "F2B" (witness :: F2B),
      test "F2C" (witness :: F2C),
      test "F2D" (witness :: F2D),
      test "F2E" (witness :: F2E),
      test "F2F" (witness :: F2F),
      test "F2G" (witness :: F2G),
      test "F2H" (witness :: F2H),
      test "F2I" (witness :: F2I)
    ]
