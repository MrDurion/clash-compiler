module Clash.Aiger.Unsigned.Bounded where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (all0BV, all1BV)
import Clash.Sized.Internal.Unsigned (Unsigned)

minBound, maxBound :: forall n. (KnownNat n) => Unsigned n
minBound = as @(Unsigned n) $ all0BV
maxBound = as @(Unsigned n) $ all1BV
