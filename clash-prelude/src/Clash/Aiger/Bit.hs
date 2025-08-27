module Clash.Aiger.Bit where

import Prelude hiding (and, or)

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (Bit)

import qualified Clash.Aiger.Base as Base
import {-# SOURCE #-} qualified Clash.Aiger.Int as INT

(+), (-), (*) :: Bit -> Bit -> Bit
negate, abs, signum :: Bit -> Bit
neq, eq, lt, le, gt, ge :: Bit -> Bit -> Bool
minBound, maxBound :: Bit
and, or, xor :: Bit -> Bit -> Bit
complement :: Bit -> Bit
zeroBits :: Bit
bit :: Int -> Bit
testBit :: Bit -> Int -> Bool
bitSizeMaybe :: Bit -> Maybe Int
bitSize, popCount :: Bit -> Int
isSigned :: Bit -> Bool
setBit
  , clearBit
  , complementBit
  , shift
  , shiftL
  , shiftR
  , rotate
  , rotateL
  , rotateR ::
    Bit -> Int -> Bit
-- Num
(+) = xor
(-) = xor
(*) = and
negate = complement
abs = id
signum = id

-- fromInteger :: Word# -> Integer -> Bit
-- fromInteger w1 i1 = if BV.lsb w2 == Base.low then BV.lsb i2 else Base.undefined##
--  where
--   w2  = as @(BitVector 64) $ W# w1
--   i2 = as @(BitVector 64) $ i1

-- Eq and Ord
neq b1 b2 = as @Bool $ neq# b1 b2
eq b1 b2 = as @Bool $ eq# b1 b2
lt b1 b2 = as @Bool $ lt# b1 b2
ge b1 b2 = as @Bool $ ge# b1 b2
gt b1 b2 = as @Bool $ gt# b1 b2
le b1 b2 = as @Bool $ le# b1 b2

eq#, neq#, lt#, gt#, le#, ge# :: Bit -> Bit -> Bit
neq# b1 b2 = xor b1 b2
eq# b1 b2 = complement $ neq# b1 b2
lt# b1 b2 = b2 `and` (complement b1)
gt# b1 b2 = b1 `and` (complement b2)
le# b1 b2 = complement $ gt# b1 b2
ge# b1 b2 = complement $ lt# b1 b2

-- Bounded
minBound = Base.low
maxBound = Base.high

-- Bit
-- Basic bit operations
and = Base.and
complement = Base.complement
or b1 b2 = complement $ (complement b1) `and` (complement b2)
xor b1 b2 = complement $ (b1 `and` b2) `or` (complement b1 `and` complement b2)

-- Bit operations with constant value
zeroBits = Base.low
bitSizeMaybe _ = Just 1
bitSize _ = 1
isSigned _ = False
rotate = const
rotateL = const
rotateR = const

-- Bit operations with Int Eq:
bit i = if i `INT.eq` 0 then Base.high else Base.low
setBit b i = if i `INT.eq` 0 then Base.high else b
clearBit b i = if i `INT.eq` 0 then Base.low else b
complementBit b i = if i `INT.eq` 0 then Base.complement b else b
testBit b i = if i `INT.eq` 0 then eq b Base.high else False
shift b i = if i `INT.eq` 0 then b else Base.low
shiftL b i = if i `INT.eq` 0 then b else Base.low
shiftR b i = if i `INT.eq` 0 then b else Base.low
popCount b = if eq b Base.low then 0 else 1
