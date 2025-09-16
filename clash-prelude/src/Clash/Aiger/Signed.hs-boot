module Clash.Aiger.Signed where

import GHC.TypeLits (KnownNat, type (+))
import Prelude hiding (and, negate, or, (*), (+), (-))

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector)
import {-# SOURCE #-} Clash.Sized.Internal.Signed (Signed)

minBound, maxBound :: (KnownNat n) => Signed n
and, or, xor :: (KnownNat n) => Signed n -> Signed n -> Signed n
complement :: (KnownNat n) => Signed n -> Signed n
zeroBits :: (KnownNat n) => Signed n
bit :: (KnownNat n) => Int -> Signed n
testBit :: (KnownNat n) => Signed n -> Int -> Bool
bitSizeMaybe :: (KnownNat n) => Signed n -> Maybe Int
bitSize, popCount :: (KnownNat n) => Signed n -> Int
isSigned :: (KnownNat n) => Signed n -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    (KnownNat n) => Signed n -> Int -> Signed n
undefined# :: (KnownNat n) => Signed n
(+), (-), (*) :: (KnownNat n) => Signed n -> Signed n -> Signed n
negate, abs, signum :: (KnownNat n) => Signed n -> Signed n
eq, neq, lt, le, gt, ge :: (KnownNat n) => Signed n -> Signed n -> Bool
resize :: forall n m. (KnownNat n, KnownNat m) => Signed n -> Signed m
truncateB :: forall n m. (KnownNat n, KnownNat m) => Signed (m + n) -> Signed n
zeroExtend :: forall a b. (KnownNat a, KnownNat b) => Signed a -> Signed (b + a)
unpack :: (KnownNat n) => BitVector n -> Signed n
pack :: (KnownNat n) => Signed n -> BitVector n
