{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Clash.Tests.Aiger.BitVector (tests, main) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Aiger.BitVector hiding (n)
import Clash.Sized.Internal.BitVector (Bit)
import Clash.Tests.Aiger.TH

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
  testGroup
    "AIGER: Bit & BitVector"
    [ testGroup "or##" $ genTruthTableTests or## 0 1 1 1
    , testGroup "xor##" $ genTruthTableTests xor## 0 1 1 0
    , testGroup "eq##" $ genTruthTableTests eq## True False False True
    , testGroup "neq##" $ genTruthTableTests neq## False True True False
    , testGroup "lt##" $ genTruthTableTests lt## False True False False
    , testGroup "le##" $ genTruthTableTests le## True True False True
    , testGroup "gt##" $ genTruthTableTests gt## False False True False
    , testGroup "ge##" $ genTruthTableTests ge## True False True True
    , testGroup "and#" $(genTests 'and# 'BV.and#)
    , testGroup "or#" $(genTests 'or# 'BV.or#)
    , testGroup "xor#" $(genTests 'xor# 'BV.xor#)
    , testGroup "neq#" $(genTests 'neq# 'BV.neq#)
    , testGroup "eq#" $(genTests 'eq# 'BV.eq#)
    , testGroup "lt#" $(genTests 'lt# 'BV.lt#)
    , testGroup "le#" $(genTests 'le# 'BV.le#)
    , testGroup "gt#" $(genTests 'gt# 'BV.gt#)
    , testGroup "ge#" $(genTests 'ge# 'BV.ge#)
    , testGroup "reduceAnd#" $(genTests 'reduceAnd# 'BV.reduceAnd#)
    , testGroup "reduceOr#" $(genTests 'reduceOr# 'BV.reduceOr#)
    , testGroup "reduceXor#" $(genTests 'reduceXor# 'BV.reduceXor#)
    , testGroup "msb#" $(genTests 'msb# 'BV.msb#)
    , testGroup "lsb#" $(genTests 'lsb# 'BV.lsb#)
    , testGroup "shiftL#" $(genTests 'shiftL# 'BV.shiftL#)
    , testGroup "shiftR#" $(genTests 'shiftR# 'BV.shiftR#)
    , testGroup "rotateL#" $(genTests 'rotateL# 'BV.rotateL#)
    , testGroup "rotateR#" $(genTests 'rotateR# 'BV.rotateR#)
    , testGroup "negate#" $(genTests 'negate# 'BV.negate#)
    , testGroup "+#" $(genTests '(+#) '(BV.+#))
    , testGroup "-#" $(genTests '(-#) '(BV.-#))
    , testGroup "*#" $(genTests '(*#) '(BV.*#))
    ]

-- Run with:
--
--    ./repld p:tests -T Clash.Tests.Aiger.BitVector.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
