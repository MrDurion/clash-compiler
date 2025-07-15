{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Util
import Clash.Annotations.Primitive (hasBlackBox)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit, BitVector)

import {-# SOURCE #-} qualified Clash.Class.BitPack.Internal as BP
import {-# SOURCE #-} qualified Clash.Sized.Internal.BitVector as BV

all1BV :: (KnownNat n) => BitVector n
all1BV = complement# $ all0BV

{-# ANN undefined## hasBlackBox #-}
{-# CLASH_OPAQUE undefined## #-}
undefined## :: Bit
undefined## = BV.Bit 1 0

undefined# :: (KnownNat n) => BitVector n
undefined# = mapBV (\_ -> undefined##) (BV.BV 0 0)

reduceAnd# :: (KnownNat n) => BitVector n -> Bit
reduceAnd# bv = foldrBV (&) BV.high bv

reduceOr# :: (KnownNat n) => BitVector n -> Bit
reduceOr# bv = foldrBV BV.or## BV.low bv

reduceXor# :: (KnownNat n) => BitVector n -> Bit
reduceXor# bv = foldrBV BV.xor## BV.low bv

msb# :: (KnownNat n) => BitVector n -> Bit
msb# bv = maybeDestructBV const BV.low bv

lsb# :: (KnownNat n) => BitVector n -> Bit
lsb# bv = maybeLDestructBV go BV.low bv
 where
  go _ a = a

shiftlBV
  , shiftrBV
  , rotatelBV
  , rotaterBV ::
    forall n. (KnownNat n) => BitVector n -> BitVector n
shiftlBV bv = maybeDestructBV go all0BV bv
 where
  go _ bs = bs BV.++# BV.pack# BV.low
shiftrBV bv = maybeLDestructBV go all0BV bv
 where
  go bs _ = BV.pack# BV.low BV.++# bs
rotatelBV bv = maybeDestructBV go all0BV bv
 where
  go b bs = bs BV.++# BV.pack# b
rotaterBV bv = maybeLDestructBV go all0BV bv
 where
  go bs b = BV.pack# b BV.++# bs

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

-- TODO should this be a sub or a primitive?
-- truncateB# ::
--   forall a b. (KnownNat a, KnownNat b) => BitVector (a + b) -> BitVector a
-- truncateB# bv = maybeBV bv go def
--  where
--   go :: (1 <= (a + b)) => BitVector (a + b) -> BitVector a
--   go bs =
--     let
--       (b1 :: BitVector (a), _ :: BitVector b) = BV.split# bs
--      in
--       b1
--   def :: BitVector a
--   def = all0BV

-- BitVectors
(+#) ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
(+#) a b = fst $ adder a b

(-#) ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
(-#) a b = a +# (negate# b)

(*#) ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
(*#) a b = shiftAdd b
 where
  shiftAdd ::
    forall m. (KnownNat m) => BitVector m -> BitVector n
  shiftAdd bv = maybeLDestructBV go def bv
   where
    go bb c = (shiftlBV (shiftAdd bb)) +# (andA c)
    def = all0BV
  andA i = mapBV (& i) a

negate# :: forall n. (KnownNat n) => BitVector n -> BitVector n
negate# bv = fst $ negateBV bv

negateBV :: forall n. (KnownNat n) => BitVector n -> (BitVector n, Bit)
negateBV bv = maybeDestructBV go (all0BV, BV.high) bv
 where
  go b bs =
    let
      (r, c) = negateBV bs
      (bitr, bitc) = halfAdder (n b) c
     in
      ((BV.pack# bitr) BV.++# r, bitc)

adder ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> (BitVector n, Bit)
adder bv1 bv2 = maybeDestructBV2 go (all0BV, BV.low) bv1 bv2
 where
  go b1 b2 bs1 bs2 =
    let
      (r, c) = adder bs1 bs2
      (bitr, bitc) = fullAdder b1 b2 c
     in
      ((BV.pack# bitr) BV.++# r, bitc)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b c = (r2, c_out)
 where
  (r1, c1) = halfAdder a b
  (r2, c2) = halfAdder c r1
  c_out = or## c1 c2

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder b1 b2 = (r, c)
 where
  c = b1 & b2
  r = b1 `xor##` b2

and# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
and# = zipWithBV (&)

complement# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n
complement# = mapBV (n)

or# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
or# = zipWithBV (or##)

xor# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
xor# = zipWithBV (xor##)

neq# ::
  (KnownNat n) => BitVector n -> BitVector n -> Bool
neq# bv1 bv2 = bitToBool $ n $ eqBV bv1 bv2

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
ge## b1 b2 = bitToBool $ n (lt### b1 b2)
gt## :: Bit -> Bit -> Bool
gt## b1 b2 = bitToBool $ gt### b1 b2
le## :: Bit -> Bit -> Bool
le## b1 b2 = bitToBool $ n (b1 `gt###` b2)

-- expressed in base
xor## :: Bit -> Bit -> Bit
xor## b1 b2 = n $ eq### b1 b2

-- base (with only AND and NOT)
eq### :: Bit -> Bit -> Bit
eq### b1 b2 = (b1 & b2) `or##` (n b1 & n b2)

or## :: Bit -> Bit -> Bit
or## b1 b2 = n $ (n b1) & (n b2)

lt### :: Bit -> Bit -> Bit
lt### b1 b2 = b2 & (n b1)

gt### :: Bit -> Bit -> Bit
gt### b1 b2 = b1 & (n b2)

n :: Bit -> Bit
n = BV.complement##

(&) :: Bit -> Bit -> Bit
(&) = BV.and##

{-# ANN bitToBool hasBlackBox #-}
{-# CLASH_OPAQUE bitToBool #-}
bitToBool :: Bit -> Bool
bitToBool b = BP.bitToBool b
