{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat, type (-), type (<=))

import {-# SOURCE #-} Clash.Class.BitPack.Internal (bitToBool)
import Clash.Promoted.Nat (SNat (..), SNatLE (..), compareSNat)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit, BitVector)

import {-# SOURCE #-} qualified Clash.Sized.Internal.BitVector as BV

maybeBV ::
  forall n d.
  (KnownNat n) =>
  ((KnownNat n, 1 <= n) => BitVector n -> d) ->
  ((KnownNat n, n <= 0) => d) ->
  BitVector n ->
  d
maybeBV f d bv = case compareSNat (SNat :: SNat n) (SNat :: SNat 0) of
  (SNatGT) -> f bv
  (SNatLE) -> d

destructBV ::
  forall n. (KnownNat n, 1 <= n) => BitVector n -> (Bit, BitVector (n - 1))
destructBV bv = (bit, bs)
 where
  (b :: BitVector 1, bs :: BitVector (n - 1)) = BV.split# bv
  bit = BV.unpack# b

rdestructBV ::
  forall n. (KnownNat n, 1 <= n) => BitVector n -> (BitVector (n - 1), Bit)
rdestructBV bv = (bs, bit)
 where
  (bs :: BitVector (n - 1), b :: BitVector 1) = BV.split# bv
  bit = BV.unpack# b

maybeDestructBV ::
  forall n d.
  (KnownNat n) =>
  ((KnownNat n, 1 <= n) => Bit -> BitVector (n - 1) -> d) ->
  d ->
  BitVector n ->
  d
maybeDestructBV f d bv = maybeBV go d bv
 where
  go :: (KnownNat n, 1 <= n) => BitVector n -> d
  go bb =
    let
      (b, bs) = destructBV bb
     in
      f b bs

maybeLDestructBV ::
  forall n d.
  (KnownNat n) =>
  ((1 <= n) => BitVector (n - 1) -> Bit -> d) ->
  d ->
  BitVector n ->
  d
maybeLDestructBV f d bv = maybeBV f' d bv
 where
  f' :: ((1 <= n) => BitVector n -> d)
  f' bb = f bs b
   where
    (bs, b) = rdestructBV bb

maybeDestructBV2 ::
  forall n d.
  (KnownNat n) =>
  ((1 <= n) => Bit -> Bit -> BitVector (n - 1) -> BitVector (n - 1) -> d) ->
  d ->
  BitVector n ->
  BitVector n ->
  d
maybeDestructBV2 f d bv1 bv2 = maybeBV f' d bv1
 where
  f' :: (KnownNat n, 1 <= n) => BitVector n -> d
  f' _ =
    let
      (bit1, bs1) = destructBV bv1
      (bit2, bs2) = destructBV bv2
     in
      f bit1 bit2 bs1 bs2

mapBV :: forall n. (KnownNat n) => (Bit -> Bit) -> BitVector n -> BitVector n
mapBV f bv = maybeDestructBV go bv bv
 where
  go b bs = BV.pack# (f b) BV.++# mapBV f bs

zipWithBV ::
  forall n.
  (KnownNat n) => (Bit -> Bit -> Bit) -> BitVector n -> BitVector n -> BitVector n
zipWithBV f bv1 bv2 = maybeDestructBV2 go bv1 bv1 bv2
 where
  go b1 b2 bs1 bs2 = BV.pack# (f b1 b2) BV.++# zipWithBV f bs1 bs2

foldrBV :: forall n b. (KnownNat n) => (Bit -> b -> b) -> b -> BitVector n -> b
foldrBV f d bv = maybeDestructBV go d bv
 where
  go b bs = f b (foldrBV f d bs)

foldlBV :: forall n b. (KnownNat n) => (Bit -> b -> b) -> b -> BitVector n -> b
foldlBV f d bv = maybeLDestructBV go d bv
 where
  go bs b = f b (foldrBV f d bs)

all0BV :: (KnownNat n) => BitVector n
all0BV = BV.BV 0 0

all1BV :: (KnownNat n) => BitVector n
all1BV = complement# $ all0BV

reduceAnd# :: (KnownNat n) => BitVector n -> Bit
reduceAnd# bv = foldrBV BV.and## BV.high bv

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

shiftlBV :: forall n. (KnownNat n) => BitVector n -> BitVector n
shiftlBV bv = maybeDestructBV go bv bv
 where
  go _ bs = bs BV.++# BV.pack# BV.low

shiftrBV :: forall n. (KnownNat n) => BitVector n -> BitVector n
shiftrBV bv = maybeLDestructBV go bv bv
 where
  go bs _ = bs BV.++# BV.pack# BV.low

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
  andA i = mapBV (BV.and## i) a

negate# :: forall n. (KnownNat n) => BitVector n -> BitVector n
negate# bv = fst $ negateBV bv

growBV ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
growBV bv = (all0BV :: BitVector (n - m)) BV.++# bv

truncateBV ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector n -> BitVector m
truncateBV bv = maybeBV go def bv
 where
  go :: (1 <= n) => BitVector n -> BitVector m
  go bs =
    let
      (_ :: BitVector (n - m), b2 :: BitVector m) = BV.split# bs
     in
      b2
  def :: BitVector m
  def = all0BV

negateBV :: forall n. (KnownNat n) => BitVector n -> (BitVector n, Bit)
negateBV bv = maybeDestructBV go (bv, BV.high) bv
 where
  go b bs =
    let
      (r, c) = negateBV bs
      (bitr, bitc) = halfAdder (BV.complement## b) c
     in
      ((BV.pack# bitr) BV.++# r, bitc)

adder ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> (BitVector n, Bit)
adder bv1 bv2 = maybeDestructBV2 go (bv1, BV.low) bv1 bv2
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
  c = BV.and## b1 b2
  r = xor## b1 b2

and# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
and# = zipWithBV (BV.and##)

complement# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n
complement# = mapBV (BV.complement##)

or# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
or# = zipWithBV (BV.or##)

xor# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
xor# = zipWithBV (BV.xor##)

neq# ::
  (KnownNat n) => BitVector n -> BitVector n -> Bool
neq# bv1 bv2 = not $ BV.eq# bv1 bv2

eq# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
eq# bv1 bv2 = bitToBool $ foldrBV (BV.and##) BV.high (zipWithBV (eq###) bv1 bv2)

lt# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
lt# bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 = if BV.eq## b1 b2 then lt# bs1 bs2 else BV.lt## b1 b2
le# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
le# bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if BV.eq## b1 b2 then le# bs1 bs2 else BV.lt## b1 b2

gt# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
gt# bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 = if BV.eq## b1 b2 then gt# bs1 bs2 else BV.gt## b1 b2
ge# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
ge# bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if BV.eq## b1 b2 then ge# bs1 bs2 else BV.gt## b1 b2

-- Bits
eq## :: Bit -> Bit -> Bool
eq## b1 b2 = bitToBool $ eq### b1 b2

neq## :: Bit -> Bit -> Bool
neq## b1 b2 = bitToBool $ xor## b1 b2

lt## :: Bit -> Bit -> Bool
lt## b1 b2 = bitToBool $ lt### b1 b2
ge## :: Bit -> Bit -> Bool
ge## b1 b2 = bitToBool $ BV.complement## $ lt### b1 b2
gt## :: Bit -> Bit -> Bool
gt## b1 b2 = bitToBool $ gt### b1 b2
le## :: Bit -> Bit -> Bool
le## b1 b2 = bitToBool $ BV.complement## $ gt### b1 b2

-- expressed in base
xor## :: Bit -> Bit -> Bit
xor## b1 b2 = BV.complement## $ eq### b1 b2

-- base (with only AND and NOT)
or## :: Bit -> Bit -> Bit
or## b1 b2 = BV.complement## $ BV.and## (BV.complement## b1) (BV.complement## b2)

eq### :: Bit -> Bit -> Bit
eq### b1 b2 = BV.and## b1 b2 `BV.and##` BV.and## (BV.complement## b1) (BV.complement## b2)

lt### :: Bit -> Bit -> Bit
lt### b1 b2 = BV.and## b2 (BV.complement## b1)

gt### :: Bit -> Bit -> Bit
gt### b1 b2 = BV.and## b1 (BV.complement## b2)
