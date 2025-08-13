module Clash.Aiger.BitVector.Bounded where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Util (all0BV, all1BV)
import  Clash.Sized.Internal.BitVector (BitVector)

minBound, maxBound :: (KnownNat n) => BitVector n
minBound = all0BV
maxBound = all1BV
