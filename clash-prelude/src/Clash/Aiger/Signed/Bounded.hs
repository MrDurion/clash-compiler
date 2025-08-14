module Clash.Aiger.Signed.Bounded where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (all0BV, all1BV, flipFirstBit)
import Clash.Sized.Internal.Signed (Signed)

minBound, maxBound :: forall n. (KnownNat n) => Signed n
minBound = as @(Signed n) $ flipFirstBit all0BV
maxBound = as @(Signed n) $ flipFirstBit all1BV
