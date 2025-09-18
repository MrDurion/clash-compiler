{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Util where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))

-- basic blackboxes imported

import Clash.Aiger.Base (as)
import Clash.Promoted.Nat (SNat (..), SNatLE (..), compareSNat)
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Aiger.Base as Base

comp ::
  forall n m d.
  (KnownNat n, KnownNat m) =>
  ((KnownNat n, KnownNat m, (m + 1) <= n) => d) ->
  ((KnownNat n, KnownNat m, n <= m) => d) ->
  d
comp f1 f2 = case compareSNat (SNat :: SNat n) (SNat :: SNat m) of
  (SNatGT) -> f1
  (SNatLE) -> f2

maybeBV ::
  forall n d.
  (KnownNat n) =>
  ((KnownNat n, 1 <= n) => d) ->
  ((KnownNat n, n <= 0) => d) ->
  d
maybeBV f d = comp @n @0 f d

destructBV ::
  forall n. (KnownNat n, 1 <= n) => BitVector n -> (Bit, BitVector (n - 1))
destructBV bv = (bit, bs)
 where
  (b :: BitVector 1, bs :: BitVector (n - 1)) = Base.split# bv
  bit = as @Bit b

rdestructBV ::
  forall n. (KnownNat n, 1 <= n) => BitVector n -> (BitVector (n - 1), Bit)
rdestructBV bv = (bs, bit)
 where
  (bs :: BitVector (n - 1), b :: BitVector 1) = Base.split# bv
  bit = as @Bit b

maybeDestructBV ::
  forall n d.
  (KnownNat n) =>
  ((KnownNat n, 1 <= n) => Bit -> BitVector (n - 1) -> d) ->
  d ->
  BitVector n ->
  d
maybeDestructBV f d bv = maybeBV @n go d
 where
  go :: (1 <= n) => d
  go =
    let
      (b, bs) = destructBV bv
     in
      f b bs

maybeRDestructBV ::
  forall n d.
  (KnownNat n) =>
  ((1 <= n) => BitVector (n - 1) -> Bit -> d) ->
  d ->
  BitVector n ->
  d
maybeRDestructBV f d bv = maybeBV @(n) f' d
 where
  f' :: ((1 <= n) => d)
  f' = f bs b
   where
    (bs, b) = rdestructBV bv

maybeDestructBV2 ::
  forall n d.
  (KnownNat n) =>
  ((1 <= n) => Bit -> Bit -> BitVector (n - 1) -> BitVector (n - 1) -> d) ->
  d ->
  BitVector n ->
  BitVector n ->
  d
maybeDestructBV2 f d bv1 bv2 = maybeBV @n f' d
 where
  f' :: (1 <= n) => d
  f' =
    let
      (bit1, bs1) = destructBV bv1
      (bit2, bs2) = destructBV bv2
     in
      f bit1 bit2 bs1 bs2

mapBV :: forall n. (KnownNat n) => (Bit -> Bit) -> BitVector n -> BitVector n
mapBV f bv = maybeDestructBV go bv bv
 where
  go b bs = as @(BitVector 1) (f b) Base.++# mapBV f bs

replaceBit :: (KnownNat n) => BitVector n -> Int -> (Bit -> Bit) -> BitVector n
replaceBit bv index bit =
  if
    | index Prelude.< 0 -> bv
    | index Prelude.== 0 -> maybeRDestructBV replaceNow bv bv
    | otherwise -> maybeRDestructBV go bv bv
 where
  go bvb b = (replaceBit bvb (index - 1) bit) Base.++# (as @(BitVector 1) b)
  replaceNow bvb b = bvb Base.++# (as @(BitVector 1) $ bit b)

getIndexBV :: (KnownNat n) => BitVector n -> Int -> Bit
getIndexBV bv i = maybeRDestructBV go Base.undefined## bv
 where
  go bvb b =
    if
      | i Prelude.< 0 -> Base.undefined##
      | i Prelude.== 0 -> b
      | otherwise -> getIndexBV bvb (i - 1)

repeatBV :: forall n. (KnownNat n) => Bit -> BitVector n
repeatBV f = mapBV (\_ -> f) 0

all0BV :: forall n. (KnownNat n) => BitVector n
all0BV = repeatBV Base.low

all1BV :: (KnownNat n) => BitVector n
all1BV = repeatBV Base.high

zipWithBV ::
  forall n.
  (KnownNat n) => (Bit -> Bit -> Bit) -> BitVector n -> BitVector n -> BitVector n
zipWithBV f bv1 bv2 = maybeDestructBV2 go all0BV bv1 bv2
 where
  go b1 b2 bs1 bs2 = as @(BitVector 1) (f b1 b2) Base.++# zipWithBV f bs1 bs2

foldrBV :: forall n b. (KnownNat n) => (Bit -> b -> b) -> b -> BitVector n -> b
foldrBV f d bv = maybeDestructBV go d bv
 where
  go b bs = f b (foldrBV f d bs)

foldlBV :: forall n b. (KnownNat n) => (Bit -> b -> b) -> b -> BitVector n -> b
foldlBV f d bv = maybeRDestructBV go d bv
 where
  go bs b = f b (foldrBV f d bs)

flipFirstBit :: (KnownNat n) => BitVector n -> BitVector n
flipFirstBit bv = maybeDestructBV des bv bv
 where
  des b bs = (as @(BitVector 1) (Base.complement b)) Base.++# bs
