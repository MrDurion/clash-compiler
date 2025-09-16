{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import Prelude hiding (negate, truncate, (*), (+), (-))

import qualified Prelude

import Clash.Aiger.Base (as, (++#))
import Clash.Aiger.Util
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.Bit as Bit
import qualified Clash.Aiger.Util as Util

-- Undefined
-- Resize
-- Implementations
-- Undefined
undefined# :: (KnownNat n) => BitVector n
undefined# = repeatBV (Base.undefined##)

-- Resize
truncateB ::
  forall a b. (KnownNat a, KnownNat b) => BitVector (a + b) -> BitVector a
truncateB bv = b
 where
  (_, b) = Base.split# bv

zeroExtend
  , signExtend ::
    forall a b.
    (KnownNat a, KnownNat b) => BitVector a -> BitVector (b + a)
zeroExtend bv = (all0BV) Base.++# bv
signExtend bv = (repeatBV (msb bv)) ++# bv

resize :: forall n m. (KnownNat n, KnownNat m) => BitVector n -> BitVector m
resize b1 = comp @n @m truncate grow b1
 where
  truncate :: (m <= n) => BitVector n -> BitVector m
  truncate = truncateB @m @(n - m)
  grow :: (n <= m) => BitVector n -> BitVector m
  grow = zeroExtend @n @(m - n)

-- Num
(+)
  , (-)
  , (*) ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
(+) a b = fst $ adder a b
(-) a b = a + (negate b)
(*) a b = shiftAdd b
 where
  shiftAdd ::
    forall m. (KnownNat m) => BitVector m -> BitVector n
  shiftAdd bv = maybeLDestructBV go def bv
   where
    go bb c = (shiftlBV Base.low (shiftAdd bb)) + (andA c)
    def = all0BV
  andA i = mapBV (`Bit.and` i) a

negate, abs, signum :: forall n. (KnownNat n) => BitVector n -> BitVector n
negate bv = fst $ negateBV bv
abs = id
signum bv = if ((reduceOr bv) `Bit.eq` Base.low) then 0 else 1

-- Util

negateBV :: forall n. (KnownNat n) => BitVector n -> (BitVector n, Bit)
negateBV bv = maybeDestructBV go (all0BV, Base.high) bv
 where
  go b bs =
    let
      (r, c) = negateBV bs
      (bitr, bitc) = halfAdder (Bit.complement b) c
     in
      ((as @(BitVector 1) bitr) Base.++# r, bitc)

adder ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> (BitVector n, Bit)
adder bv1 bv2 = maybeDestructBV2 go (all0BV, Base.low) bv1 bv2
 where
  go b1 b2 bs1 bs2 =
    let
      (r, c) = adder bs1 bs2
      (bitr, bitc) = fullAdder b1 b2 c
     in
      ((as @(BitVector 1) bitr) Base.++# r, bitc)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b c = (r2, c_out)
 where
  (r1, c1) = halfAdder a b
  (r2, c2) = halfAdder c r1
  c_out = Bit.or c1 c2

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder b1 b2 = (r, c)
 where
  c = b1 `Bit.and` b2
  r = b1 `Bit.xor` b2

-- Eq
neq, eq :: forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
neq bv1 bv2 = as @Bool $ Bit.complement $ eq# bv1 bv2
eq bv1 bv2 = as @Bool $ eq# bv1 bv2

eq# :: (KnownNat n) => BitVector n -> BitVector n -> Bit
eq# bv1 bv2 = reduceAnd (zipWithBV (Bit.eq#) bv1 bv2)

-- Ord
lt
  , le
  , gt
  , ge ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
lt bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 =
    let
      rest = lt bs1 bs2
      firstBitEqual = Bit.eq b1 b2
      firstBitLt = Bit.lt b1 b2
     in
      if firstBitEqual then rest else firstBitLt
le bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then le bs1 bs2 else Bit.lt b1 b2
gt bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then gt bs1 bs2 else Bit.gt b1 b2
ge bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then ge bs1 bs2 else Bit.gt b1 b2

-- Bounded
minBound, maxBound :: forall n. (KnownNat n) => BitVector n
minBound = all0BV
maxBound = all1BV

-- Bits
and
  , or
  , xor ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
complement :: forall n. (KnownNat n) => BitVector n -> BitVector n
zeroBits :: forall n. (KnownNat n) => BitVector n
bit :: forall n. (KnownNat n) => Int -> BitVector n
testBit :: forall n. (KnownNat n) => BitVector n -> Int -> Bool
bitSizeMaybe :: forall n. (KnownNat n) => BitVector n -> Maybe Int
bitSize, popCount :: forall n. (KnownNat n) => BitVector n -> Int
isSigned :: (KnownNat n) => BitVector n -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    (KnownNat n) => BitVector n -> Int -> BitVector n
and = zipWithBV Bit.and
complement = mapBV (Bit.complement)
or = zipWithBV Bit.or
xor = zipWithBV Bit.xor

zeroBits = all0BV
bit i = Util.replaceBit all0BV i (const Base.high)
setBit bv i = Util.replaceBit bv i (const Base.high)
clearBit bv i = Util.replaceBit bv i (const Base.low)
complementBit bv i = Util.replaceBit bv i (\bt -> Bit.complement bt)
testBit bv i = (getIndexBV bv i) `Bit.eq` Base.high
bitSizeMaybe bv = Just (bitSize bv)
bitSize bv = foldrBV go 0 bv
 where
  go :: a -> Int -> Int
  go _ (i :: Int) = i Prelude.+ 1
isSigned _ = False
popCount bv = foldrBV go 0 bv
 where
  go b i = if Bit.eq b Base.high then i Prelude.+ 1 else i

shiftL bv i =
  if
    | i < 0 -> undefined#
    | i == 0 -> bv
    | otherwise -> shiftL (shiftlBV Base.low bv) (i Prelude.- 1)
shiftR bv i =
  if
    | i < 0 -> undefined#
    | i == 0 -> bv
    | otherwise -> shiftR (shiftrBV Base.low bv) (i Prelude.- 1)
rotateL bv i =
  if
    | i < 0 -> undefined#
    | i == 0 -> bv
    | otherwise -> rotateL (rotatelBV bv) (i Prelude.- 1)
rotateR bv i =
  if
    | i < 0 -> undefined#
    | i == 0 -> bv
    | otherwise -> rotateR (rotaterBV bv) (i Prelude.- 1)

shiftlBV
  , shiftrBV ::
    forall n. (KnownNat n) => Bit -> BitVector n -> BitVector n
shiftlBV replacementBit bv = maybeDestructBV go all0BV bv
 where
  go _ bs = bs ++# low
  low = as @(BitVector 1) replacementBit
shiftrBV replacementBit bv = maybeLDestructBV go all0BV bv
 where
  go bs _ = low ++# bs
  low = as @(BitVector 1) replacementBit

rotatelBV
  , rotaterBV ::
    forall n. (KnownNat n) => BitVector n -> BitVector n
rotatelBV bv = maybeDestructBV go all0BV bv
 where
  go b bs = bs ++# as @(BitVector 1) b
rotaterBV bv = maybeLDestructBV go all0BV bv
 where
  go bs b = as @(BitVector 1) b ++# bs

-- Extra
reduceAnd, reduceOr, reduceXor :: (KnownNat n) => BitVector n -> Bit
reduceAnd bv = foldrBV Bit.and Base.high bv
reduceOr bv = foldrBV Bit.or Base.low bv
reduceXor bv = foldrBV Bit.xor Base.low bv

msb, lsb :: (KnownNat n) => BitVector n -> Bit
msb bv = maybeDestructBV go Base.low bv
 where
  go a _ = a
lsb bv = maybeLDestructBV go Base.low bv
 where
  go _ a = a
