module Clash.Aiger.Bool (
  -- Bits
  and,
  or,
  xor,
  complement,
  zeroBits,
  bit,
  testBit,
  bitSizeMaybe,
  bitSize,
  popCount,
  isSigned,
  setBit,
  clearBit,
  complementBit,
  shift,
  shiftL,
  shiftR,
  rotate,
  rotateL,
  rotateR,
  -- BOunded
  minBound,
  maxBound,
  -- EqOrd
  eq,
  neq,
  lt,
  le,
  gt,
  ge,
  -- Num
  (+),
  (-),
  (*),
  negate,
  abs,
  signum,
) where

import Prelude hiding (
  abs,
  and,
  maxBound,
  minBound,
  negate,
  or,
  signum,
  (*),
  (+),
  (-),
 )

import Clash.Aiger.Bool.Bits
import Clash.Aiger.Bool.Bounded
import Clash.Aiger.Bool.EqOrd
import Clash.Aiger.Bool.Num
