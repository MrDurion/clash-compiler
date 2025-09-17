{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Signed where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import Prelude hiding (and, negate, or, truncate, (*), (+), (-))

import qualified Prelude

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (all0BV, all1BV, comp, flipFirstBit)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.Bit as Bit
import qualified Clash.Aiger.BitVector as BV
import qualified Clash.Aiger.Util as Util

unpack :: forall n. (KnownNat n) => BitVector n -> Signed n
pack :: forall n. (KnownNat n) => Signed n -> BitVector n
(+), (-), (*) :: forall n. (KnownNat n) => Signed n -> Signed n -> Signed n
negate, abs, signum :: forall n. (KnownNat n) => Signed n -> Signed n
eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    forall n. (KnownNat n) => Signed n -> Signed n -> Bool
minBound, maxBound :: forall n. (KnownNat n) => Signed n
and, or, xor :: forall n. (KnownNat n) => Signed n -> Signed n -> Signed n
complement :: forall n. (KnownNat n) => Signed n -> Signed n
zeroBits :: forall n. (KnownNat n) => Signed n
bit :: forall n. (KnownNat n) => Int -> Signed n
testBit :: forall n. (KnownNat n) => Signed n -> Int -> Bool
bitSizeMaybe :: forall n. (KnownNat n) => Signed n -> Maybe Int
bitSize, popCount :: forall n. (KnownNat n) => Signed n -> Int
isSigned :: forall n. (KnownNat n) => Signed n -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => Signed n -> Int -> Signed n
undefined# :: forall n. (KnownNat n) => Signed n
-- Implementations
-- BitPack
unpack = as @(Signed n) @(BitVector n)
pack = as @(BitVector n) @(Signed n)

-- Undefined
undefined# = as @(Signed n) BV.undefined#

-- Resize
resize :: forall m n. (KnownNat n, KnownNat m) => Signed n -> Signed m
resize s = comp @n @m (truncate s) (grow s)
 where
  truncate :: (m <= n) => Signed n -> Signed m
  truncate (as @(BitVector n) -> bv) = as @(Signed m) final
   where
    truncatedBV :: BitVector m
    truncatedBV = BV.truncateB @m @(n - m) bv
    signBit = BV.msb bv
    replaceFirstBitWithSignBit _ bs = (as @(BitVector 1) signBit) Base.++# bs
    final = Util.maybeDestructBV replaceFirstBitWithSignBit all0BV truncatedBV
  grow :: (n <= m) => Signed n -> Signed m
  grow = signExtend @n @(m - n)

truncateB :: forall m n. (KnownNat m, KnownNat n) => Signed (m + n) -> Signed m
truncateB (as @(BitVector (m + n)) -> bv) = as @(Signed m) $ BV.truncateB bv

zeroExtend :: forall a b. (KnownNat a, KnownNat b) => Signed a -> Signed (b + a)
zeroExtend (as @(BitVector a) -> bv) = as @(Signed (b + a)) $ BV.zeroExtend bv

signExtend :: forall a b. (KnownNat a, KnownNat b) => Signed a -> Signed (b + a)
signExtend (as @(BitVector a) -> bv) = as @(Signed (b + a)) $ BV.signExtend bv

-- Num
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.* bv2
negate (as @(BitVector n) -> bv) = as @(Signed n) $ BV.negate bv
abs s = if lt s 0 then negate s else s
signum (as @(BitVector n) -> bv) =
  as @(Signed n) $
    if
      | BV.eq bv BV.zeroBits -> BV.zeroBits -- = 0
      | ((BV.msb bv) `Bit.eq` Base.high) -> all1BV -- = -1
      | otherwise -> 1

-- Eq and Ord
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2

lt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.lt (flipFirstBit bv1) (flipFirstBit bv2)
le (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.le (flipFirstBit bv1) (flipFirstBit bv2)
gt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.gt (flipFirstBit bv1) (flipFirstBit bv2)
ge (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.ge (flipFirstBit bv1) (flipFirstBit bv2)

-- Bounded
minBound = as @(Signed n) $ flipFirstBit all0BV
maxBound = as @(Signed n) $ flipFirstBit all1BV

-- Bits
and (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.and bv1 bv2
or (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.or bv1 bv2
xor (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.xor bv1 bv2
complement (as @(BitVector n) -> bv) = as @(Signed n) $ BV.complement bv
zeroBits = as @(Signed n) BV.zeroBits
bit i = as @(Signed n) $ BV.bit i
testBit (as @(BitVector n) -> bv) = BV.testBit bv
bitSizeMaybe (as @(BitVector n) -> bv) = BV.bitSizeMaybe bv
bitSize (as @(BitVector n) -> bv) = BV.bitSize bv
setBit (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.setBit bv i
clearBit (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.clearBit bv i
complementBit (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.complementBit bv i
rotateL (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.rotateL bv i
rotateR (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.rotateR bv i
popCount (as @(BitVector n) -> bv) = BV.popCount bv
shiftL (as @(BitVector n) -> bv) i = as @(Signed n) $ BV.shiftL bv i

-- is different
isSigned _ = True
shiftR (as @(BitVector n) -> bv) i =
  if
    | i < 0 -> undefined#
    | i == 0 -> as @(Signed n) bv
    | otherwise -> shiftR (as @(Signed n) $ BV.shiftrBV signBit bv) (i Prelude.- 1)
 where
  signBit = BV.msb bv
