{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat, type (-))

import Clash.Promoted.Nat (SNat (..), SNatLE (..), compareSNat)
import {-# SOURCE #-} Clash.Sized.Internal.BitVector

xor# :: forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
xor# bv1 bv2 = case compareSNat (SNat :: SNat n) (SNat :: SNat 0) of
  SNatGT ->
    let
      (b1 :: BitVector 1, bs1 :: BitVector (n - 1)) = split# bv1
      (b2 :: BitVector 1, bs2 :: BitVector (n - 1)) = split# bv2
     in
      pack# (unpack# b1 `xor##` unpack# b2) ++# xor# bs1 bs2
  SNatLE -> bv1
