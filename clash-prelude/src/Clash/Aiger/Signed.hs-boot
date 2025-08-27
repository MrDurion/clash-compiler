module Clash.Aiger.Signed where

import GHC.TypeLits (KnownNat)
import Prelude hiding (and, negate, or, (*), (+), (-))

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector)
import {-# SOURCE #-} Clash.Sized.Internal.Signed (Signed)

and, or, xor :: (KnownNat n) => Signed n -> Signed n -> Signed n
complement :: (KnownNat n) => Signed n -> Signed n
(+), (-), (*) :: (KnownNat n) => Signed n -> Signed n -> Signed n
negate :: (KnownNat n) => Signed n -> Signed n
eq, neq, lt, le, gt, ge :: (KnownNat n) => Signed n -> Signed n -> Bool
resize :: forall n m. (KnownNat n, KnownNat m) => Signed n -> Signed m
unpack :: forall n. (KnownNat n) => BitVector n -> Signed n
pack :: forall n. (KnownNat n) => Signed n -> BitVector n
