module Clash.Aiger.Bool where

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

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (Bit)

import qualified Clash.Aiger.Bit as Bit

(+), (-), (*) :: Bool -> Bool -> Bool
negate, abs, signum :: Bool -> Bool
eq, neq, lt, le, gt, ge :: Bool -> Bool -> Bool
minBound, maxBound :: Bool
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
-- Num
(+) (as @Bit -> b1) (as @Bit -> b2) = as @Bool $ (Bit.+) b1 b2
(-) (as @Bit -> b1) (as @Bit -> b2) = as @Bool $ (Bit.-) b1 b2
(*) (as @Bit -> b1) (as @Bit -> b2) = as @Bool $ (Bit.*) b1 b2
negate (as @Bit -> b) = as @Bool $ Bit.negate b
abs (as @Bit -> b) = as @Bool $ Bit.abs b
signum (as @Bit -> b) = as @Bool $ Bit.signum b

-- Eq and Ord
eq (as @Bit -> b1) (as @Bit -> b2) = Bit.eq b1 b2
neq (as @Bit -> b1) (as @Bit -> b2) = Bit.neq b1 b2
lt (as @Bit -> b1) (as @Bit -> b2) = Bit.lt b1 b2
le (as @Bit -> b1) (as @Bit -> b2) = Bit.le b1 b2
gt (as @Bit -> b1) (as @Bit -> b2) = Bit.gt b1 b2
ge (as @Bit -> b1) (as @Bit -> b2) = Bit.ge b1 b2

-- Bounded
minBound = as @Bool Bit.minBound
maxBound = as @Bool Bit.maxBound

-- Bits
-- Basics
and b1 b2 = as @Bool $ Bit.and (as @Bit b1) (as @Bit b2)
or b1 b2 = as @Bool $ Bit.or (as @Bit b1) (as @Bit b2)
xor b1 b2 = as @Bool $ Bit.xor (as @Bit b1) (as @Bit b2)
complement b = as @Bool $ Bit.complement (as @Bit b)

-- Bit operations with constant value
zeroBits = as @Bool $ Bit.zeroBits
bitSizeMaybe b = Bit.bitSizeMaybe (as @Bit b)
bitSize b = Bit.bitSize (as @Bit b)
isSigned b = Bit.isSigned (as @Bit b)
rotate b i = as @Bool $ Bit.rotate (as @Bit b) i
rotateL b i = as @Bool $ Bit.rotateL (as @Bit b) i
rotateR b i = as @Bool $ Bit.rotateR (as @Bit b) i

-- Bit operations with Int Eq:
bit i = as @Bool $ Bit.bit i
setBit b i = as @Bool $ Bit.setBit (as @Bit b) i
clearBit b i = as @Bool $ Bit.clearBit (as @Bit b) i
complementBit b i = as @Bool $ Bit.complementBit (as @Bit b) i
testBit b i = Bit.testBit (as @Bit b) i
shift b i = as @Bool $ Bit.shift (as @Bit b) i
shiftL b i = as @Bool $ Bit.shiftL (as @Bit b) i
shiftR b i = as @Bool $ Bit.shiftR (as @Bit b) i
popCount b = Bit.popCount (as @Bit b)
