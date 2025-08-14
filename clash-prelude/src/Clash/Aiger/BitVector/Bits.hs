{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector.Bits where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as, (++#))
import Clash.Aiger.Util (
  all0BV,
  foldrBV,
  mapBV,
  maybeDestructBV,
  maybeLDestructBV,
  zipWithBV,
 )
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.Bit.Bits as Bit

and
  , or
  , xor ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
and = zipWithBV Base.and
or = zipWithBV Bit.or
xor = zipWithBV Bit.xor

complement :: (KnownNat n) => BitVector n -> BitVector n
complement = mapBV (Base.complement)

reduceAnd, reduceOr, reduceXor :: (KnownNat n) => BitVector n -> Bit
reduceAnd bv = foldrBV Base.and Base.high bv
reduceOr bv = foldrBV Bit.or Base.low bv
reduceXor bv = foldrBV Bit.xor Base.low bv

msb, lsb :: (KnownNat n) => BitVector n -> Bit
msb bv = maybeDestructBV go Base.low bv
 where
  go a _ = a
lsb bv = maybeLDestructBV go Base.low bv
 where
  go _ a = a

shiftlBV
  , shiftrBV
  , rotatelBV
  , rotaterBV ::
    forall n. (KnownNat n) => BitVector n -> BitVector n
shiftlBV bv = maybeDestructBV go all0BV bv
 where
  go _ bs = bs ++# low
  low = as @(BitVector 1) Base.low
shiftrBV bv = maybeLDestructBV go all0BV bv
 where
  go bs _ = low ++# bs
  low = as @(BitVector 1) Base.low
rotatelBV bv = maybeDestructBV go all0BV bv
 where
  go b bs = bs ++# as @(BitVector 1) b
rotaterBV bv = maybeLDestructBV go all0BV bv
 where
  go bs b = as @(BitVector 1) b ++# bs

-- I use the Haskell native version of integer comparison here, since that does not
-- make a cyclic dependency.
--
-- I want to use this system of going the haskell route for every occurance where a
-- function of a datatype requires stuff of a datatype of a higher level
--
-- required higher functions: Int Num and EqOrd
shiftL
  , shiftR
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => BitVector n -> Int -> BitVector n
shiftL bv i =
  if
    | i < 0 ->
        -- TODO , only synthesizable code
        error $ "'shiftL' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        shiftL (shiftlBV bv) (i - 1)
shiftR bv i =
  if
    | i < 0 ->
        -- TODO , only synthesizable code
        error $ "'shiftR' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        shiftR (shiftrBV bv) (i - 1)
rotateL bv i =
  if
    | i < 0 ->
        -- TODO , only synthesizable code
        error $ "'rotateL' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        rotateL (rotatelBV bv) (i - 1)
rotateR bv i =
  if
    | i < 0 ->
        -- TODO , only synthesizable code
        error $ "'rotateR' undefined for negative number: " ++ show i
    | i == 0 ->
        bv
    | otherwise ->
        rotateR (rotaterBV bv) (i - 1)
