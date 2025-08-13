module Clash.Aiger.Bit (
  -- EqOrd
  eq,
  neq,
  lt,
  le,
  gt,
  ge,
  -- Bits
  or,
  xor,
) where

import Prelude hiding (or)
import {-#SOURCE#-} Clash.Sized.Internal.BitVector (Bit)

eq, neq, lt, le, gt, ge :: Bit -> Bit -> Bool
or, xor :: Bit -> Bit -> Bit
