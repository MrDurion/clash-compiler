{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Signed where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))

import Clash.Aiger.Util (comp)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector)
import {-# SOURCE #-} Clash.Sized.Internal.Signed (
  Signed,
  pack#,
  unpack#,
 )

import qualified Clash.Aiger.BitVector as BV (
  and#,
  complement#,
  growBV,
  or#,
  truncateB#,
  xor#,
  (*#),
  (+#),
  (-#),
 )

inBV ::
  (KnownNat m, KnownNat n) =>
  (BitVector m -> BitVector n) -> Signed m -> Signed n
inBV f a = unpack# $ f (pack# a)

inBV2 ::
  (KnownNat n) =>
  (BitVector n -> BitVector n -> BitVector n) ->
  Signed n ->
  Signed n ->
  Signed n
inBV2 f a b = unpack# $ pack# a `f` pack# b

-- Substitutions

xor# :: (KnownNat n) => Signed n -> Signed n -> Signed n
xor# = inBV2 BV.xor#

or# :: (KnownNat n) => Signed n -> Signed n -> Signed n
or# = inBV2 BV.or#

and# :: (KnownNat n) => Signed n -> Signed n -> Signed n
and# = inBV2 BV.and#

complement# :: (KnownNat n) => Signed n -> Signed n
complement# = inBV BV.complement#

(-#) :: (KnownNat n) => Signed n -> Signed n -> Signed n
(-#) = inBV2 (BV.-#)

(+#) :: (KnownNat n) => Signed n -> Signed n -> Signed n
(+#) = inBV2 (BV.+#)

(*#) :: (KnownNat n) => Signed n -> Signed n -> Signed n
(*#) = inBV2 (BV.*#)

-- TODO find impl for resizing signed numbers
resize# :: forall n m. (KnownNat n, KnownNat m) => Signed n -> Signed m
resize# = inBV go
 where
  go :: BitVector n -> BitVector m
  go = comp @n @m trunc grow

  trunc ::
    (m <= n) => BitVector (m + (n - m)) -> BitVector m
  trunc = BV.truncateB#

  grow :: (n <= m) => BitVector n -> BitVector m
  grow = BV.growBV
