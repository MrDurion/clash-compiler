module Clash.Aiger.Bit.Bounded where

import Clash.Sized.Internal.BitVector (Bit)

import qualified Clash.Aiger.Base as Base

minBound, maxBound :: Bit
minBound = Base.low
maxBound = Base.high
