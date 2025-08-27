module Clash.Aiger.Bit where

import GHC.Base (Word#)
import Prelude hiding (and, or)

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit)

-- fromInteger :: Word# -> Integer -> Bit
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
