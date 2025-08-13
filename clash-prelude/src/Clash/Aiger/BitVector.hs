module Clash.Aiger.BitVector (
  -- EqOrd
  eq,
  neq,
  lt,
  le,
  gt,
  ge,
  -- Bounded
  minBound,
  maxBound,
  -- Num
  (+),
  (-),
  (*),
  negate,
  -- Bits
  and,
  complement,
  or,
  xor,
  reduceAnd,
  reduceOr,
  reduceXor,
  msb,
  lsb,
  shiftL,
  shiftR,
  rotateL,
  rotateR,
  -- Resize
  truncateB,
  -- Undefined
  undefined#,
) where

import Prelude hiding (and, maxBound, minBound, negate, or, (*), (+), (-))

import Clash.Aiger.BitVector.Undefined
import Clash.Aiger.BitVector.Bits
import Clash.Aiger.BitVector.Bounded
import Clash.Aiger.BitVector.EqOrd
import Clash.Aiger.BitVector.Num
import Clash.Aiger.BitVector.Resize
