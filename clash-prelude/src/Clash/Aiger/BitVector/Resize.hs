{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector.Resize where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))

import Clash.Aiger.Base ((++#))
import Clash.Aiger.Util (all0BV, repeatBV)
import Clash.Sized.Internal.BitVector (BitVector)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.BitVector.Bits as BV

truncateB :: forall a b. (KnownNat a) => BitVector (a + b) -> BitVector a
truncateB bv = b
 where
  (_, b) = Base.split# bv

zeroExtend ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
zeroExtend bv = (all0BV :: BitVector (n - m)) Base.++# bv

signExtend ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
signExtend bv = (repeatBV msb :: BitVector (n - m)) ++# bv
 where
  msb = BV.msb bv
