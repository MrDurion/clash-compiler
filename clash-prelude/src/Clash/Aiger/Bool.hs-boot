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

and, or, xor :: Bool -> Bool -> Bool
complement :: Bool -> Bool
zeroBits :: Bool
bit :: Int -> Bool
testBit :: Bool -> Int -> Bool
bitSizeMaybe :: Bool -> Maybe Int
bitSize, popCount :: Bool -> Int
isSigned :: Bool -> Bool
setBit
  , clearBit
  , complementBit
  , shift
  , shiftL
  , shiftR
  , rotate
  , rotateL
  , rotateR ::
    Bool -> Int -> Bool
minBound, maxBound :: Bool
eq, neq, lt, le, gt, ge :: Bool -> Bool -> Bool
(+), (-), (*) :: Bool -> Bool -> Bool
negate, abs, signum :: Bool -> Bool
