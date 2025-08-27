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

-- Util
toBit :: Bool -> Bit
toBit b = as @Bit @Bool b

asBool :: Bit -> Bool
asBool b = as @Bool @Bit b

-- Num
(+), (-), (*) :: Bool -> Bool -> Bool
negate, abs, signum :: Bool -> Bool
(+) (toBit -> b1) (toBit -> b2) = asBool $ (Bit.+) b1 b2
(-) (toBit -> b1) (toBit -> b2) = asBool $ (Bit.-) b1 b2
(*) (toBit -> b1) (toBit -> b2) = asBool $ (Bit.*) b1 b2
negate (toBit -> b) = asBool $ Bit.negate b
abs (toBit -> b) = asBool $ Bit.abs b
signum (toBit -> b) = asBool $ Bit.signum b

-- Eq and Ord
eq, neq, lt, le, gt, ge :: Bool -> Bool -> Bool
eq (toBit -> b1) (toBit -> b2) = Bit.eq b1 b2
neq (toBit -> b1) (toBit -> b2) = Bit.neq b1 b2
lt (toBit -> b1) (toBit -> b2) = Bit.lt b1 b2
le (toBit -> b1) (toBit -> b2) = Bit.le b1 b2
gt (toBit -> b1) (toBit -> b2) = Bit.gt b1 b2
ge (toBit -> b1) (toBit -> b2) = Bit.ge b1 b2

-- Bounded
minBound, maxBound :: Bool
minBound = as @Bool Bit.minBound
maxBound = as @Bool Bit.maxBound

-- Bits
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
-- Basics
and b1 b2 = asBool $ Bit.and (toBit b1) (toBit b2)
or b1 b2 = asBool $ Bit.or (toBit b1) (toBit b2)
xor b1 b2 = asBool $ Bit.xor (toBit b1) (toBit b2)
complement b = asBool $ Bit.complement (toBit b)

-- Bit operations with constant value
zeroBits = asBool $ Bit.zeroBits
bitSizeMaybe b = Bit.bitSizeMaybe (toBit b)
bitSize b = Bit.bitSize (toBit b)
isSigned b = Bit.isSigned (toBit b)
rotate b i = asBool $ Bit.rotate (toBit b) i
rotateL b i = asBool $ Bit.rotateL (toBit b) i
rotateR b i = asBool $ Bit.rotateR (toBit b) i

-- Bit operations with Int Eq:
bit i = asBool $ Bit.bit i
setBit b i = asBool $ Bit.setBit (toBit b) i
clearBit b i = asBool $ Bit.clearBit (toBit b) i
complementBit b i = asBool $ Bit.complementBit (toBit b) i
testBit b i = Bit.testBit (toBit b) i
shift b i = asBool $ Bit.shift (toBit b) i
shiftL b i = asBool $ Bit.shiftL (toBit b) i
shiftR b i = asBool $ Bit.shiftR (toBit b) i
popCount b = Bit.popCount (toBit b)
