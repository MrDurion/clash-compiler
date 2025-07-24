{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import GHC.TypeLits.Extra (Max)

-- From Util layer
import Clash.Aiger.Util (
  all0BV,
  all1BV,
  foldrBV,
  mapBV,
  maybeDestructBV,
  maybeDestructBV2,
  maybeLDestructBV,
  repeatBV,
  undefined##,
  zipWithBV,
 )
-- packing import
import {-# SOURCE #-} Clash.Class.BitPack.Internal (bitToBool)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit, BitVector)

-- imported blackboxes for Bit and BitVector
import {-# SOURCE #-} qualified Clash.Sized.Internal.BitVector as BB_BV (
  and##,
  complement##,
  high,
  low,
  pack#,
  split#,
  (++#),
 )

reduceAnd# :: (KnownNat n) => BitVector n -> Bit
reduceAnd# bv = foldrBV BB_BV.and## BB_BV.high bv

reduceOr# :: (KnownNat n) => BitVector n -> Bit
reduceOr# bv = foldrBV or## BB_BV.low bv

reduceXor# :: (KnownNat n) => BitVector n -> Bit
reduceXor# bv = foldrBV xor## BB_BV.low bv

msb# :: (KnownNat n) => BitVector n -> Bit
msb# bv = maybeDestructBV go BB_BV.low bv
 where
  go a _ = a

lsb# :: (KnownNat n) => BitVector n -> Bit
lsb# bv = maybeLDestructBV go BB_BV.low bv
 where
  go _ a = a

shiftlBV
  , shiftrBV
  , rotatelBV
  , rotaterBV ::
    forall n. (KnownNat n) => BitVector n -> BitVector n
shiftlBV bv = maybeDestructBV go all0BV bv
 where
  go _ bs = bs BB_BV.++# BB_BV.pack# BB_BV.low
shiftrBV bv = maybeLDestructBV go all0BV bv
 where
  go bs _ = BB_BV.pack# BB_BV.low BB_BV.++# bs
rotatelBV bv = maybeDestructBV go all0BV bv
 where
  go b bs = bs BB_BV.++# BB_BV.pack# b
rotaterBV bv = maybeLDestructBV go all0BV bv
 where
  go bs b = BB_BV.pack# b BB_BV.++# bs

-- TODO only use synthesizable code
shiftL#
  , shiftR#
  , rotateL#
  , rotateR# ::
    forall n. (KnownNat n) => BitVector n -> Int -> BitVector n
shiftL# bv i =
  if
    | i < 0 ->
        error $ "'shiftL' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        shiftL# (shiftlBV bv) (i - 1)
shiftR# bv i =
  if
    | i < 0 ->
        error $ "'shiftR' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        shiftR# (shiftrBV bv) (i - 1)
rotateL# bv i =
  if
    | i < 0 ->
        error $ "'rotateL' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        rotateL# (rotatelBV bv) (i - 1)
rotateR# bv i =
  if
    | i < 0 ->
        error $ "'rotateR' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        rotateR# (rotaterBV bv) (i - 1)

-- BitVectors
growBV ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
growBV bv = (all0BV :: BitVector (n - m)) BB_BV.++# bv

plus# ::
  forall m n.
  (KnownNat m, KnownNat n, m <= Max m n, n <= Max m n) =>
  BitVector m -> BitVector n -> BitVector (Max m n + 1)
plus# a b = a1 +# b1
 where
  a1 = growBV a
  b1 = growBV b

minus# ::
  forall m n.
  (KnownNat m, KnownNat n, m <= Max m n, n <= Max m n) =>
  BitVector m ->
  BitVector n ->
  BitVector
    (Max m n + 1)
minus# a b = a1 -# b1
 where
  a1 = growBV a
  b1 = growBV b

truncateB# :: forall a b. (KnownNat a) => BitVector (a + b) -> BitVector a
truncateB# bv = b
 where
  (_, b) = BB_BV.split# bv

minBound# :: (KnownNat n) => BitVector n
minBound# = all0BV

maxBound# :: (KnownNat n) => BitVector n
maxBound# = all1BV

undefined# :: (KnownNat n) => BitVector n
undefined# = repeatBV (undefined##)

times# ::
  forall m n.
  (KnownNat m, KnownNat n) => BitVector m -> BitVector n -> BitVector (m + n)
times# a b = a1 *# b1
 where
  a1 = growBV a
  b1 = growBV b

(-#)
  , (+#)
  , (/#)
  , (*#)
  , (%#) ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
(+#) a b = fst $ adder a b
(-#) a b = a +# (negate# b)
(/#) a b = undefined
(%#) a b = undefined
(*#) a b = shiftAdd b
 where
  shiftAdd ::
    forall m. (KnownNat m) => BitVector m -> BitVector n
  shiftAdd bv = maybeLDestructBV go def bv
   where
    go bb c = (shiftlBV (shiftAdd bb)) +# (andA c)
    def = all0BV
  andA i = mapBV (`BB_BV.and##` i) a

negate# :: forall n. (KnownNat n) => BitVector n -> BitVector n
negate# bv = fst $ negateBV bv

negateBV :: forall n. (KnownNat n) => BitVector n -> (BitVector n, Bit)
negateBV bv = maybeDestructBV go (all0BV, BB_BV.high) bv
 where
  go b bs =
    let
      (r, c) = negateBV bs
      (bitr, bitc) = halfAdder (BB_BV.complement## b) c
     in
      ((BB_BV.pack# bitr) BB_BV.++# r, bitc)

adder ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> (BitVector n, Bit)
adder bv1 bv2 = maybeDestructBV2 go (all0BV, BB_BV.low) bv1 bv2
 where
  go b1 b2 bs1 bs2 =
    let
      (r, c) = adder bs1 bs2
      (bitr, bitc) = fullAdder b1 b2 c
     in
      ((BB_BV.pack# bitr) BB_BV.++# r, bitc)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b c = (r2, c_out)
 where
  (r1, c1) = halfAdder a b
  (r2, c2) = halfAdder c r1
  c_out = or## c1 c2

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder b1 b2 = (r, c)
 where
  c = b1 `BB_BV.and##` b2
  r = b1 `xor##` b2

and# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
and# = zipWithBV BB_BV.and##

complement# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n
complement# = mapBV (BB_BV.complement##)

or# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
or# = zipWithBV (or##)

xor# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
xor# = zipWithBV (xor##)

neq# ::
  (KnownNat n) => BitVector n -> BitVector n -> Bool
neq# bv1 bv2 = bitToBool $ BB_BV.complement## $ eqBV bv1 bv2

eq# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
eq# bv1 bv2 = bitToBool $ eqBV bv1 bv2

eqBV :: (KnownNat n) => BitVector n -> BitVector n -> Bit
eqBV bv1 bv2 = reduceAnd# (zipWithBV (eq###) bv1 bv2)

lt# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
lt# bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 = if eq## b1 b2 then lt# bs1 bs2 else lt## b1 b2
le# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
le# bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if eq## b1 b2 then le# bs1 bs2 else lt## b1 b2

gt# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
gt# bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 = if eq## b1 b2 then gt# bs1 bs2 else gt## b1 b2
ge# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
ge# bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if eq## b1 b2 then ge# bs1 bs2 else gt## b1 b2

-- Bits
eq## :: Bit -> Bit -> Bool
eq## b1 b2 = bitToBool $ eq### b1 b2

neq## :: Bit -> Bit -> Bool
neq## b1 b2 = bitToBool $ xor## b1 b2

lt## :: Bit -> Bit -> Bool
lt## b1 b2 = bitToBool $ lt### b1 b2
ge## :: Bit -> Bit -> Bool
ge## b1 b2 = bitToBool $ BB_BV.complement## (lt### b1 b2)
gt## :: Bit -> Bit -> Bool
gt## b1 b2 = bitToBool $ gt### b1 b2
le## :: Bit -> Bit -> Bool
le## b1 b2 = bitToBool $ BB_BV.complement## (b1 `gt###` b2)

-- expressed in base
xor## :: Bit -> Bit -> Bit
xor## b1 b2 = BB_BV.complement## $ eq### b1 b2

-- base (with only AND and NOT)
eq### :: Bit -> Bit -> Bit
eq### b1 b2 =
  (b1 `BB_BV.and##` b2)
    `or##` (BB_BV.complement## b1 `BB_BV.and##` BB_BV.complement## b2)

or## :: Bit -> Bit -> Bit
or## b1 b2 =
  BB_BV.complement## $
    (BB_BV.complement## b1) `BB_BV.and##` (BB_BV.complement## b2)

lt### :: Bit -> Bit -> Bit
lt### b1 b2 = b2 `BB_BV.and##` (BB_BV.complement## b1)

gt### :: Bit -> Bit -> Bit
gt### b1 b2 = b1 `BB_BV.and##` (BB_BV.complement## b2)
