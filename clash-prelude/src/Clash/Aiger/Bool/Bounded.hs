module Clash.Aiger.Bool.Bounded where

import Clash.Aiger.Base (as)

import qualified Clash.Aiger.Bit.Bounded as Bit

minBound, maxBound :: Bool
minBound = as @Bool Bit.minBound
maxBound = as @Bool Bit.maxBound
