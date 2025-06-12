{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat, type (-))

import Clash.Promoted.Nat (SNat (..), SNatLE (..), compareSNat)
import {-# SOURCE #-} qualified Clash.Sized.Internal.BitVector as BV
import {-# SOURCE #-} Clash.Sized.Internal.BitVector (BitVector)

xor# ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
xor# bv1 bv2 = case compareSNat (SNat :: SNat n) (SNat :: SNat 0) of
  (SNatGT) ->
    let
      (b1 :: BitVector 1, bs1 :: BitVector (n - 1)) = BV.split# bv1
      (b2 :: BitVector 1, bs2 :: BitVector (n - 1)) = BV.split# bv2
     in
      BV.pack# (BV.unpack# b1 `BV.xor##` BV.unpack# b2) BV.++# xor# bs1 bs2
  (SNatLE) -> bv1
