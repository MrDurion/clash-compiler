{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Signed where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import Prelude hiding (and, negate, or, truncate, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (all0BV, all1BV)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.Bit as Bit
import qualified Clash.Aiger.BitVector as BV
import {-# SOURCE #-} qualified Clash.Aiger.Int as INT
import qualified Clash.Aiger.Util as Util

--
-- Util function
flipFirstBit :: (KnownNat n) => BitVector n -> BitVector n
flipFirstBit bv = Util.maybeDestructBV flipBit bv bv
 where
  flipBit b bs = (as @(BitVector 1) (Base.complement b)) Base.++# bs

-- BitPack
unpack :: forall n. (KnownNat n) => BitVector n -> Signed n
pack :: forall n. (KnownNat n) => Signed n -> BitVector n
unpack = as @(Signed n) @(BitVector n)
pack = as @(BitVector n) @(Signed n)

-- Undefined
undefined# :: forall n. (KnownNat n) => Signed n
undefined# = as @(Signed n) BV.undefined#

-- Resize
resize :: forall m n. (KnownNat n, KnownNat m) => Signed n -> Signed m
resize s = Util.comp @n @m (truncate s) (grow s)
 where
  grow :: (n <= m) => Signed n -> Signed m
  grow = signExtend @n @(m - n)
  truncate :: (m <= n) => Signed n -> Signed m
  truncate (as @(BitVector n) -> bv) = as @(Signed m) shrunkenWithFirstBitReplaced
   where
    truncatedBV :: BitVector m
    truncatedBV = BV.truncateB @m @(n - m) bv
    signBit = BV.msb bv
    replaceFirstBitWithSignBit _ bs = (as @(BitVector 1) signBit) Base.++# bs
    shrunkenWithFirstBitReplaced = Util.maybeDestructBV replaceFirstBitWithSignBit all0BV truncatedBV

truncateB :: forall m n. (KnownNat m, KnownNat n) => Signed (n + m) -> Signed m
truncateB (as @(BitVector (n + m)) -> bv) = as @(Signed m) $ BV.truncateB bv

zeroExtend :: forall a b. (KnownNat a, KnownNat b) => Signed a -> Signed (b + a)
zeroExtend (as @(BitVector a) -> bv) = as @(Signed (b + a)) $ BV.zeroExtend bv

signExtend :: forall a b. (KnownNat a, KnownNat b) => Signed a -> Signed (b + a)
signExtend (as @(BitVector a) -> bv) = as @(Signed (b + a)) $ BV.signExtend bv

-- Eq and Ord
eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    forall n. (KnownNat n) => Signed n -> Signed n -> Bool
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2
lt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.lt (flipFirstBit bv1) (flipFirstBit bv2)
le (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.le (flipFirstBit bv1) (flipFirstBit bv2)
gt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.gt (flipFirstBit bv1) (flipFirstBit bv2)
ge (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.ge (flipFirstBit bv1) (flipFirstBit bv2)

-- Num
(+), (-), (*) :: forall n. (KnownNat n) => Signed n -> Signed n -> Signed n
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.* bv2

negate :: forall n. (KnownNat n) => Signed n -> Signed n
negate (as @(BitVector n) -> bv) = as @(Signed n) $ BV.negate bv

abs :: forall n. (KnownNat n) => Signed n -> Signed n
abs s = if s `lt` 0 then negate s else s

signum :: forall n. (KnownNat n) => Signed n -> Signed n
signum (as @(BitVector n) -> bv) =
  if
    | bv `BV.eq` all0BV -> 0
    | ((BV.msb bv) `Bit.eq` Base.high) -> as @(Signed n) all1BV -- signed number with all bits 1 is `-1`
    | otherwise -> 1

-- Bounded
minBound, maxBound :: forall n. (KnownNat n) => Signed n
minBound = as @(Signed n) $ flipFirstBit all0BV
maxBound = as @(Signed n) $ flipFirstBit all1BV

-- Bits

and, or, xor :: forall n. (KnownNat n) => Signed n -> Signed n -> Signed n
and (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.and bv1 bv2
or (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.or bv1 bv2
xor (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.xor bv1 bv2

complement :: forall n. (KnownNat n) => Signed n -> Signed n
complement (as @(BitVector n) -> bv) = as @(Signed n) $ BV.complement bv

zeroBits :: forall n. (KnownNat n) => Signed n
zeroBits = as @(Signed n) BV.zeroBits

bit :: forall n. (KnownNat n) => Int -> Signed n
bit i = as @(Signed n) $ BV.bit i

testBit :: forall n. (KnownNat n) => Signed n -> Int -> Bool
testBit (as @(BitVector n) -> bv) = BV.testBit bv

bitSizeMaybe :: forall n. (KnownNat n) => Signed n -> Maybe Int
bitSizeMaybe (as @(BitVector n) -> bv) = BV.bitSizeMaybe bv

bitSize :: forall n. (KnownNat n) => Signed n -> Int
bitSize (as @(BitVector n) -> bv) = BV.bitSize bv

setBit
  , clearBit
  , complementBit ::
    forall n. (KnownNat n) => Signed n -> Int -> Signed n
setBit (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.setBit bv i
clearBit (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.clearBit bv i
complementBit (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.complementBit bv i

popCount :: forall n. (KnownNat n) => Signed n -> Int
popCount (as @(BitVector n) -> bv) = BV.popCount bv

-- different from Unsigned
isSigned :: forall n. (KnownNat n) => Signed n -> Bool
isSigned _ = True

shiftL
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => Signed n -> Int -> Signed n
rotateL (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.rotateL bv i
rotateR (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.rotateR bv i
shiftL (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.shiftL bv i

-- different from Unsigned
shiftR :: forall n. (KnownNat n) => Signed n -> Int -> Signed n
shiftR (as @(BitVector n) -> bv) i =
  if
    | i `INT.lt` 0 -> undefined#
    | i `INT.eq` 0 -> as @(Signed n) bv
    | otherwise -> shiftR (as @(Signed n) $ BV.shiftrBV signBit bv) (i INT.- 1)
 where
  signBit = BV.msb bv
