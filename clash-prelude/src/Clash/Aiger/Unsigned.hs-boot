module Clash.Aiger.Unsigned (
  -- Bits
  and,
  or,
  xor,
  complement,
  -- Num
  (+),
  (-),
  (*),
  negate,
  -- Resize
  resize,
  -- EqOrd
  eq,
  neq,
  lt,
  le,
  gt,
  ge,
)
where

import GHC.TypeLits (KnownNat)
import Prelude hiding (and, negate, or, (*), (+), (-))

import {-# SOURCE #-} Clash.Sized.Internal.Unsigned (Unsigned)

and, or, xor :: (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
complement :: (KnownNat n) => Unsigned n -> Unsigned n
(+), (-), (*) :: (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
negate :: (KnownNat n) => Unsigned n -> Unsigned n
eq, neq, lt, le, gt, ge :: (KnownNat n) => Unsigned n -> Unsigned n -> Bool
resize :: forall n m. (KnownNat n, KnownNat m) => Unsigned n -> Unsigned m
