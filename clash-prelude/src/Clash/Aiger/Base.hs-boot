module Clash.Aiger.Base where

import GHC.TypeLits (KnownNat, type (+))

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit, BitVector)

high :: Bit
low :: Bit
split# ::
  forall n m.
  (KnownNat n) =>
  BitVector (m + n) ->
  (BitVector m, BitVector n)
(++#) :: (KnownNat m) => BitVector n -> BitVector m -> BitVector (n + m)
