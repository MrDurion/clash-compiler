module Clash.Aiger.Bit.Bits where

import  Clash.Sized.Internal.BitVector (Bit)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.Bit.EqOrd as Bit

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
-- Basic bit operations
and = Base.and
or = Base.or
xor = Base.xor
complement = Base.complement

-- Bit operations with constant value
zeroBits = Base.low
bitSizeMaybe _ = Just 1
bitSize _ = 1
isSigned _ = False
rotate = const
rotateL = const
rotateR = const

-- Bit operations with Int Eq:
bit i = if i == 0 then Base.high else Base.low
setBit b i = if i == 0 then Base.high else b
clearBit b i = if i == 0 then Base.low else b
complementBit b i = if i == 0 then Base.complement b else b
testBit b i = if i == 0 then Bit.eq b Base.high else False
shift b i = if i == 0 then b else Base.low
shiftL b i = if i == 0 then b else Base.low
shiftR b i = if i == 0 then b else Base.low
popCount b = if Bit.eq b Base.low then 0 else 1
