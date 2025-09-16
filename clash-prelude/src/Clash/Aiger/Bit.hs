module Clash.Aiger.Bit where

import Prelude hiding (and, or)

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Aiger.Base as Base
import {-# SOURCE #-} qualified Clash.Aiger.Int as INT

-- Num
(+), (-), (*) :: Bit -> Bit -> Bit
(+) = xor
(-) = xor
(*) = and
negate, abs, signum :: Bit -> Bit
negate = id
abs = id
signum = id

-- BitPack
pack :: Bit -> BitVector 1
pack = as @(BitVector 1)

unpack :: BitVector 1 -> Bit
unpack = as @Bit

-- Eq and Ord
neq, eq, lt, le, gt, ge :: Bit -> Bit -> Bool
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
minBound, maxBound :: Bit
minBound = Base.low
maxBound = Base.high

-- Bits
-- Basic bit operations
and, or, xor :: Bit -> Bit -> Bit
and = Base.and
or b1 b2 = complement $ (complement b1) `and` (complement b2)
xor b1 b2 = complement $ (b1 `and` b2) `or` (complement b1 `and` complement b2)

complement :: Bit -> Bit
complement = Base.complement

-- Bit operations with constant value
zeroBits :: Bit
zeroBits = Base.low
bitSizeMaybe :: Bit -> Maybe Int
bitSizeMaybe _ = Just 1
bitSize :: Bit -> Int
bitSize _ = 1
isSigned :: Bit -> Bool
isSigned _ = False
rotate, rotateL, rotateR :: Bit -> Int -> Bit
rotate = const
rotateL = const
rotateR = const

-- Bit operations with Int Eq:
bit :: Int -> Bit
bit i = if i `INT.eq` 0 then Base.high else Base.low
testBit :: Bit -> Int -> Bool
testBit b i = if i `INT.eq` 0 then eq b Base.high else False
popCount :: Bit -> Int
popCount b = if eq b Base.low then 0 else 1

setBit
  , clearBit
  , complementBit
  , shift
  , shiftL
  , shiftR ::
    Bit -> Int -> Bit
setBit b i = if i `INT.eq` 0 then Base.high else b
clearBit b i = if i `INT.eq` 0 then Base.low else b
complementBit b i = if i `INT.eq` 0 then Base.complement b else b
shift b i = if i `INT.eq` 0 then b else Base.low
shiftL b i = if i `INT.eq` 0 then b else Base.low
shiftR b i = if i `INT.eq` 0 then b else Base.low
