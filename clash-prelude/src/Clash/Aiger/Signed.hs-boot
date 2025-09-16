module Clash.Aiger.Signed where

import GHC.TypeLits (KnownNat, type (+))
import Prelude hiding (and, negate, or, (*), (+), (-))

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector)
import {-# SOURCE #-} Clash.Sized.Internal.Signed (Signed)

minBound, maxBound :: forall n. (KnownNat n) => Signed n
and, or, xor :: (KnownNat n) => Signed n -> Signed n -> Signed n
complement :: (KnownNat n) => Signed n -> Signed n
zeroBits :: forall n. (KnownNat n) => Signed n
bit :: forall n. (KnownNat n) => Int -> Signed n
testBit :: forall n. (KnownNat n) => Signed n -> Int -> Bool
bitSizeMaybe :: forall n. (KnownNat n) => Signed n -> Maybe Int
bitSize, popCount :: forall n. (KnownNat n) => Signed n -> Int
isSigned :: forall n. (KnownNat n) => Signed n -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => Signed n -> Int -> Signed n
undefined# :: forall n. (KnownNat n) => Signed n
(+), (-), (*) :: (KnownNat n) => Signed n -> Signed n -> Signed n
negate, abs, signum :: forall n. (KnownNat n) => Signed n -> Signed n
eq, neq, lt, le, gt, ge :: (KnownNat n) => Signed n -> Signed n -> Bool
resize :: forall n m. (KnownNat n, KnownNat m) => Signed n -> Signed m
truncateB :: forall a b. (KnownNat a, KnownNat b) => Signed(a + b) -> Signed a
zeroExtend :: forall a b. (KnownNat a, KnownNat b) => Signed a -> Signed (b + a)
unpack :: forall n. (KnownNat n) => BitVector n -> Signed n
pack :: forall n. (KnownNat n) => Signed n -> BitVector n
