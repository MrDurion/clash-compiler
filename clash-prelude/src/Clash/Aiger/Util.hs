{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Util where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Promoted.Nat (SNat (..), SNatLE (..), compareSNat)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit, BitVector)

import {-# SOURCE #-} qualified Clash.Sized.Internal.BitVector as BV

maybeBV ::
  forall n d.
  (KnownNat n) =>
  ((KnownNat n, 1 <= n) => d) ->
  ((KnownNat n, n <= 0) => d) ->
  d
maybeBV f d = case compareSNat (SNat :: SNat n) (SNat :: SNat 0) of
  (SNatGT) -> f
  (SNatLE) -> d

destructBV ::
  forall n. (KnownNat n, 1 <= n) => BitVector n -> (Bit, BitVector (n - 1))
destructBV bv = (bit, bs)
 where
  -- bs :: BitVector (n - 1) = lastBV @1 bv
  -- b :: BitVector (1) = firstBV @(n - 1) bv
  (b :: BitVector 1, bs :: BitVector (n - 1)) = BV.split# bv
  bit = BV.unpack# b

rdestructBV ::
  forall n. (KnownNat n, 1 <= n) => BitVector n -> (BitVector (n - 1), Bit)
rdestructBV bv = (bs, bit)
 where
  -- bs :: BitVector (n - 1) = firstBV @1 bv
  -- b :: BitVector (1) = lastBV @(n - 1) bv
  (bs :: BitVector (n - 1), b :: BitVector 1) = BV.split# bv
  bit = BV.unpack# b

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

maybeLDestructBV ::
  forall n d.
  (KnownNat n) =>
  ((1 <= n) => BitVector (n - 1) -> Bit -> d) ->
  d ->
  BitVector n ->
  d
maybeLDestructBV f d bv = maybeBV @(n) f' d
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
  f' :: (KnownNat n, 1 <= n) => d
  f' =
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
zipWithBV f bv1 bv2 = maybeDestructBV2 go all0BV bv1 bv2
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

{-# ANN all0BV hasBlackBox #-}
{-# CLASH_OPAQUE all0BV #-}
all0BV :: (KnownNat n) => BitVector n
all0BV = BV.BV 0 0

growBV ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
growBV bv = (all0BV :: BitVector (n - m)) BV.++# bv
