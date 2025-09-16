{-# LANGUAGE CPP #-}

module Clash.Aiger.Base where

import GHC.TypeLits (KnownNat, type (+))
import Prelude hiding (and, or)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Class.BitPack (BitPack, BitSize)
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Sized.Internal.BitVector as BitVector (
  Bit (..),
  and##,
  complement##,
  high,
  low,
  split#,
  (++#),
 )

{-# ANN high hasBlackBox #-}
{-# CLASH_OPAQUE high #-}
high :: Bit
high = BitVector.high

{-# ANN low hasBlackBox #-}
{-# CLASH_OPAQUE low #-}
low :: Bit
low = BitVector.low

{-# ANN and hasBlackBox #-}
{-# CLASH_OPAQUE and #-}
and :: Bit -> Bit -> Bit
and = BitVector.and##

{-# ANN complement hasBlackBox #-}
{-# CLASH_OPAQUE complement #-}
complement :: Bit -> Bit
complement = BitVector.complement##

{-# ANN undefined## hasBlackBox #-}
{-# CLASH_OPAQUE undefined## #-}
undefined## :: Bit
undefined## = BitVector.Bit 1 0

{-# ANN split# hasBlackBox #-}
{-# CLASH_OPAQUE split# #-}
split# ::
  forall n m.
  (KnownNat n) =>
  BitVector (m + n) ->
  (BitVector m, BitVector n)
split# = BitVector.split#

{-# ANN (++#) hasBlackBox #-}
{-# CLASH_OPAQUE (++#) #-}
(++#) :: (KnownNat m) => BitVector n -> BitVector m -> BitVector (n + m)
(++#) = (BitVector.++#)

{-# ANN as hasBlackBox #-}
{-# CLASH_OPAQUE as #-}
as ::
  forall r n.
  (BitSize n ~ BitSize r) =>
  n -> r
as = undefined
