module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat, type (+))
import Prelude hiding (and, maxBound, minBound, negate, or, (*), (+), (-))

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit, BitVector)

eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    (KnownNat n) => BitVector n -> BitVector n -> Bool
minBound, maxBound :: (KnownNat n) => BitVector n
(+)
  , (-)
  , (*) ::
    (KnownNat n) => BitVector n -> BitVector n -> BitVector n
negate, abs, signum :: (KnownNat n) => BitVector n -> BitVector n
and
  , or
  , xor ::
    (KnownNat n) => BitVector n -> BitVector n -> BitVector n
complement :: (KnownNat n) => BitVector n -> BitVector n
reduceAnd, reduceOr, reduceXor :: (KnownNat n) => BitVector n -> Bit
msb, lsb :: (KnownNat n) => BitVector n -> Bit
undefined# :: (KnownNat n) => BitVector n
zeroBits :: (KnownNat n) => BitVector n
bit :: (KnownNat n) => Int -> BitVector n
testBit :: (KnownNat n) => BitVector n -> Int -> Bool
bitSizeMaybe :: (KnownNat n) => BitVector n -> Maybe Int
bitSize, popCount :: (KnownNat n) => BitVector n -> Int
isSigned :: (KnownNat n) => BitVector n -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    (KnownNat n) => BitVector n -> Int -> BitVector n
truncateB ::
  forall a b. (KnownNat a, KnownNat b) => BitVector (a + b) -> BitVector a
zeroExtend
  , signExtend ::
    (KnownNat a, KnownNat b) => BitVector a -> BitVector (b + a)
resize :: forall n m. (KnownNat n, KnownNat m) => BitVector n -> BitVector m
