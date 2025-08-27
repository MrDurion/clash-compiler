{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import Prelude hiding (negate, truncate, (+))

import Clash.Aiger.Base (as, (++#))
import Clash.Aiger.Util
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.Bit as Bit
import {-# SOURCE #-} qualified Clash.Aiger.Int as INT

-- Undefined
undefined# :: (KnownNat n) => BitVector n
undefined# = repeatBV (Base.undefined##)

-- Resize
truncateB :: forall a b. (KnownNat a) => BitVector (a + b) -> BitVector a
truncateB bv = b
 where
  (_, b) = Base.split# bv

zeroExtend ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
zeroExtend bv = (all0BV :: BitVector (n - m)) Base.++# bv

signExtend ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => BitVector m -> BitVector n
signExtend bv = (repeatBV (msb bv) :: BitVector (n - m)) ++# bv

resize :: forall n m. (KnownNat n, KnownNat m) => BitVector n -> BitVector m
resize b1 = comp @n @m truncate grow b1
 where
  truncate ::
    (m <= n) => BitVector (m + (n - m)) -> BitVector m
  truncate = truncateB

  grow :: (n <= m) => BitVector n -> BitVector m
  grow = zeroExtend

-- #####
-- BitVector NumWithResize or something already, not in scope tho
--
-- plus# ::
--   forall m n.
--   (KnownNat m, KnownNat n, m <= Max m n, n <= Max m n) =>
--   BitVector m -> BitVector n -> BitVector (Max m n + 1)
-- plus# a b = a1 +# b1
--  where
--   a1 = growBV a
--   b1 = growBV b
--
-- minus# ::
--   forall m n.
--   (KnownNat m, KnownNat n, m <= Max m n, n <= Max m n) =>
--   BitVector m ->
--   BitVector n ->
--   BitVector
--     (Max m n + 1)
-- minus# a b = a1 -# b1
--  where
--   a1 = growBV a
--   b1 = growBV b
--
-- times# ::
--   forall m n.
--   (KnownNat m, KnownNat n) => BitVector m -> BitVector n -> BitVector (m + n)
-- times# a b = a1 *# b1
--  where
--   a1 = growBV a
--   b1 = growBV b

-- Num BitVector
(-)
  , (+)
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
    go bb c = (shiftlBV (shiftAdd bb)) + (andA c)
    def = all0BV
  andA i = mapBV (`Bit.and` i) a

negate :: forall n. (KnownNat n) => BitVector n -> BitVector n
negate bv = fst $ negateBV bv

-- fromInteger :: (KnownNat n) => Integer -> Integer -> BitVector n
-- fromInteger (as @(Int) -> i1) (as @(Int) -> i2) = resize $ zipWithBV z i1 i2
--  where
--   z b1 b2 = if b1 == Base.low then b2 else Base.undefined##

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
neq, eq :: (KnownNat n) => BitVector n -> BitVector n -> Bool
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
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then lt bs1 bs2 else Bit.lt b1 b2
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
minBound, maxBound :: (KnownNat n) => BitVector n
minBound = all0BV
maxBound = all1BV

-- Bits
and
  , or
  , xor ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
and = zipWithBV Bit.and
or = zipWithBV Bit.or
xor = zipWithBV Bit.xor

complement :: (KnownNat n) => BitVector n -> BitVector n
complement = mapBV (Bit.complement)

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
    | i `INT.lt` 0 -> undefined#
    | i `INT.eq` 0 -> bv
    | otherwise -> shiftL (shiftlBV bv) (i INT.- 1)
shiftR bv i =
  if
    | i `INT.lt` 0 -> undefined#
    | i `INT.eq` 0 -> bv
    | otherwise -> shiftR (shiftrBV bv) (i INT.- 1)
rotateL bv i =
  if
    | i `INT.lt` 0 -> undefined#
    | i `INT.eq` 0 -> bv
    | otherwise -> rotateL (rotatelBV bv) (i INT.- 1)
rotateR bv i =
  if
    | i `INT.lt` 0 -> undefined#
    | i `INT.eq` 0 -> bv
    | otherwise -> rotateR (rotaterBV bv) (i INT.- 1)
