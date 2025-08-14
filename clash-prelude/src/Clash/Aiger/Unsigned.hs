module Clash.Aiger.Unsigned (
  -- Bits
  and,
  or,
  xor,
  complement,
  -- Num
  (+),
  (-),
  (*),
  negate,
  -- Resize
  resize,
  -- EqOrd
  eq,
  neq,
  lt,
  le,
  gt,
  ge,
)
where

import Prelude hiding (and, negate, or, (*), (+), (-))

import Clash.Aiger.Unsigned.Bits
import Clash.Aiger.Unsigned.EqOrd
import Clash.Aiger.Unsigned.Num
import Clash.Aiger.Unsigned.Resize
