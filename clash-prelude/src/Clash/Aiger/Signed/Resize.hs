{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Signed.Resize where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))

import Clash.Aiger.Util (comp, as)
import Clash.Sized.Internal.BitVector (BitVector)
import  Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.BitVector.Resize as BV

resize :: forall n m. (KnownNat n, KnownNat m) => Signed n -> Signed m
resize (as @(BitVector n) -> bv)  = as @(Signed m) $ go bv
 where
  go :: BitVector n -> BitVector m
  go = comp @n @m trunc grow

  trunc ::
    (m <= n) => BitVector (m + (n - m)) -> BitVector m
  trunc = BV.truncateB

  grow :: (n <= m) => BitVector n -> BitVector m
  grow = BV.growBV
