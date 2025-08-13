{-# LANGUAGE CPP #-}

module Clash.Aiger.Base where

import GHC.TypeLits (KnownNat, type (+))
import Prelude hiding (and, or)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Sized.Internal.BitVector as BB_B (
  Bit (..),
  and##,
  complement##,
  high,
  low,
  pack#,
  split#,
  unpack#,
  (++#),
 )

{-# ANN high hasBlackBox #-}
{-# CLASH_OPAQUE high #-}
high :: Bit
high = BB_B.high

{-# ANN low hasBlackBox #-}
{-# CLASH_OPAQUE low #-}
low :: Bit
low = BB_B.low

{-# ANN and hasBlackBox #-}
{-# CLASH_OPAQUE and #-}
and :: Bit -> Bit -> Bit
and = BB_B.and##

{-# ANN complement hasBlackBox #-}
{-# CLASH_OPAQUE complement #-}
complement :: Bit -> Bit
complement = BB_B.complement##

{-# ANN undefined## hasBlackBox #-}
{-# CLASH_OPAQUE undefined## #-}
undefined## :: Bit
undefined## = BB_B.Bit 1 0

{-# ANN split# hasBlackBox #-}
{-# CLASH_OPAQUE split# #-}
split# ::
  forall n m.
  (KnownNat n) =>
  BitVector (m + n) ->
  (BitVector m, BitVector n)
split# = BB_B.split#

{-# ANN (++#) hasBlackBox #-}
{-# CLASH_OPAQUE (++#) #-}
(++#) :: (KnownNat m) => BitVector n -> BitVector m -> BitVector (n + m)
(++#) = (BB_B.++#)

{-# ANN pack# hasBlackBox #-}
{-# CLASH_OPAQUE pack# #-}
pack# :: Bit -> BitVector 1
pack# = BB_B.pack#

{-# ANN unpack# hasBlackBox #-}
{-# CLASH_OPAQUE unpack# #-}
unpack# :: BitVector 1 -> Bit
unpack# = BB_B.unpack#

-- Basic logic gates from the primitives

or :: Bit -> Bit -> Bit
or b1 b2 = complement $ (complement b1) `and` (complement b2)
xor :: Bit -> Bit -> Bit
xor b1 b2 = complement $ (b1 `and` b2) `or` (complement b1 `and` complement b2)
