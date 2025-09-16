module Clash.Aiger.Unsigned where

import GHC.TypeLits (KnownNat, type (+))
import Prelude hiding (and, negate, or, (*), (+), (-))

import {-# SOURCE #-} Clash.Sized.Internal.Unsigned (Unsigned)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector)

unpack :: forall n. (KnownNat n) => BitVector n -> Unsigned n
pack :: forall n. (KnownNat n) => Unsigned n -> BitVector n
minBound, maxBound :: forall n. (KnownNat n) => Unsigned n
(+), (-), (*) :: (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
negate, abs, signum :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
eq, neq, lt, le, gt, ge :: (KnownNat n) => Unsigned n -> Unsigned n -> Bool
resize :: forall n m. (KnownNat n, KnownNat m) => Unsigned n -> Unsigned m
zeroExtend :: forall a b. (KnownNat a, KnownNat b) => Unsigned a -> Unsigned (b + a)
truncateB :: forall a b. (KnownNat a, KnownNat b) => Unsigned (a + b) -> Unsigned a
and, or, xor :: (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
complement :: (KnownNat n) => Unsigned n -> Unsigned n
zeroBits :: forall n. (KnownNat n) => Unsigned n
bit :: forall n. (KnownNat n) => Int -> Unsigned n
testBit :: forall n. (KnownNat n) => Unsigned n -> Int -> Bool
bitSizeMaybe :: forall n. (KnownNat n) => Unsigned n -> Maybe Int
bitSize, popCount :: forall n. (KnownNat n) => Unsigned n -> Int
isSigned :: (KnownNat n) => Unsigned n -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => Unsigned n -> Int -> Unsigned n
