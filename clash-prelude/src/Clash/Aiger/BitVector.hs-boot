module Clash.Aiger.BitVector where

import Prelude hiding (and, maxBound, minBound, negate, or, (*), (+), (-))
import GHC.TypeLits (KnownNat, type (+))
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector, Bit)

eq,
  neq,
  lt,
  le,
  gt,
  ge :: KnownNat n => BitVector n -> BitVector n -> Bool

minBound, maxBound :: KnownNat n => BitVector n
(+),
  (-),
  (*) :: KnownNat n => BitVector n -> BitVector n -> BitVector n
negate :: KnownNat n => BitVector n -> BitVector n
  -- Bits
and,
  or,
  xor :: KnownNat n => BitVector n -> BitVector n -> BitVector n
complement :: KnownNat n => BitVector n -> BitVector n
reduceAnd, reduceOr, reduceXor :: (KnownNat n) => BitVector n -> Bit
msb, lsb :: (KnownNat n) => BitVector n -> Bit
shiftL
  , shiftR
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => BitVector n -> Int -> BitVector n
truncateB :: forall a b. (KnownNat a) => BitVector (a + b) -> BitVector a
undefined# :: (KnownNat n) => BitVector n
