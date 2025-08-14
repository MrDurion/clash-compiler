{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Unsigned.Resize where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (comp)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Aiger.BitVector.Resize as BV

resize :: forall n m. (KnownNat n, KnownNat m) => Unsigned n -> Unsigned m
resize (as @(BitVector n) -> bv) = as @(Unsigned m) $ go bv
 where
  go :: BitVector n -> BitVector m
  go = comp @n @m trunc grow

  trunc ::
    (m <= n) => BitVector (m + (n - m)) -> BitVector m
  trunc = BV.truncateB

  grow :: (n <= m) => BitVector n -> BitVector m
  grow = BV.zeroExtend
