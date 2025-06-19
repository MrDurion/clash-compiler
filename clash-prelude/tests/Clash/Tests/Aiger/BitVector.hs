{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Clash.Tests.Aiger.BitVector (tests, main) where

import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Aiger.BitVector
import Clash.Sized.Internal.BitVector (Bit, BitVector (..))

import qualified Clash.Sized.Internal.BitVector as BV

genTruthTableTests ::
  (Show a, Eq a) => (Bit -> Bit -> a) -> a -> a -> a -> a -> [TestTree]
genTruthTableTests f oo oi io ii =
  [ testCase ("0 0 = " ++ show oo) $ (BV.low `f` BV.low) @?= oo
  , testCase ("0 1 = " ++ show oi) $ (BV.low `f` BV.high) @?= oi
  , testCase ("1 0 = " ++ show io) $ (BV.high `f` BV.low) @?= io
  , testCase ("1 1 = " ++ show ii) $ (BV.high `f` BV.high) @?= ii
  ]

tests :: TestTree
tests =
  localOption (Q.QuickCheckMaxRatio 2) $
    testGroup
      "All"
      [ testGroup
          "Truth tables"
          [ testGroup "or##" $ genTruthTableTests or## 0 1 1 1
          , testGroup "xor##" $ genTruthTableTests xor## 0 1 1 0
          , testGroup "eq##" $ genTruthTableTests eq## True False False True
          , testGroup "neq##" $ genTruthTableTests neq## False True True False
          , testGroup "lt##" $ genTruthTableTests lt## False True False False
          , testGroup "le##" $ genTruthTableTests le## True True False True
          , testGroup "gt##" $ genTruthTableTests gt## False False True False
          , testGroup "ge##" $ genTruthTableTests ge## True False True True
          ]
          -- , testGroup
          --     "BitVector logic"
          --     [ testCase "and#" $ test1 0b00000000 @?= 0
          --     , testCase "or#" $ test1 0b00000000 @?= 0
          --     , testCase "xor#" $ test1 0b00000000 @?= 0
          --     , testCase "neq#" $ test1 0b00000000 @?= 0
          --     , testCase "eq#" $ test1 0b00000000 @?= 0
          --     , testCase "lt#" $ test1 0b00000000 @?= 0
          --     , testCase "le#" $ test1 0b00000000 @?= 0
          --     , testCase "gt#" $ test1 0b00000000 @?= 0
          --     , testCase "ge#" $ test1 0b00000000 @?= 0
          --     ]
          -- , testGroup
          --     "BitVector manipulation"
          --     [ testCase "reduceAnd#" $ test1 0b01111111 @?= 0
          --     , testCase "reduceOr#" $ test1 0b01111111 @?= 0
          --     , testCase "reduceXor#" $ test1 0b01100000 @?= 0
          --     , testCase "msb#" $ test1 0b11111101 @?= 2
          --     , testCase "lsb#" $ test1 0b11100001 @?= 2
          --     , testCase "shiftL#" $ test1 0b11111110 @?= 3
          --     , testCase "shiftR#" $ test1 0b11111111 @?= 4
          --     , testCase "rotateL#" $ test1 0b11010110 @?= 9
          --     , testCase "rotateR#" $ test1 0b11010110 @?= 9
          --     ]
          -- , testGroup
          --     "BitVector arithmetic"
          --     [ testCase "negate#" $ test1 0b11010110 @?= 9
          --     , testCase "+#" $ test1 0b11010110 @?= 9
          --     , testCase "-#" $ test1 0b11010110 @?= 9
          --     , testCase "*#" $ test1 0b11010110 @?= 9
          --     ]
      ]

-- Run with:
--
--    ./repld p:tests -T Clash.Tests.Aiger.BitVector.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
