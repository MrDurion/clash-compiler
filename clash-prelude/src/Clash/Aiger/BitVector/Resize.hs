{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector.Resize where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))

import Clash.Aiger.Util (all0BV)
import  Clash.Sized.Internal.BitVector (BitVector)

import qualified Clash.Aiger.Base as Base

truncateB :: forall a b. (KnownNat a) => BitVector (a + b) -> BitVector a
truncateB bv = b
 where
  (_, b) = Base.split# bv

growBV ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
growBV bv = (all0BV :: BitVector (n - m)) Base.++# bv
