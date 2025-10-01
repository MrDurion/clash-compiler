{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Unsigned where

import GHC.TypeLits (KnownNat, type (+))
import Prelude hiding (and, negate, or, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Aiger.BitVector as BV

-- Implementations

-- BitPack
unpack :: forall n. (KnownNat n) => BitVector n -> Unsigned n
pack :: forall n. (KnownNat n) => Unsigned n -> BitVector n
unpack = as @(Unsigned n) @(BitVector n)
pack = as @(BitVector n) @(Unsigned n)

-- Resize
truncateB ::
  forall a b. (KnownNat a, KnownNat b) => Unsigned (b + a) -> Unsigned a
truncateB (as @(BitVector (b + a)) -> bv) = as @(Unsigned a) $ BV.truncateB bv

zeroExtend
  , signExtend ::
    forall a b. (KnownNat a, KnownNat b) => Unsigned a -> Unsigned (b + a)
signExtend (as @(BitVector a) -> bv) = as @(Unsigned (b + a)) $ BV.zeroExtend bv
zeroExtend (as @(BitVector a) -> bv) = as @(Unsigned (b + a)) $ BV.zeroExtend bv

resize :: forall n m. (KnownNat n, KnownNat m) => Unsigned n -> Unsigned m
resize (as @(BitVector n) -> bv) = as @(Unsigned m) $ BV.resize bv

-- Undefined
undefined# :: forall n. (KnownNat n) => Unsigned n
undefined# = as @(Unsigned n) BV.undefined#

-- Num
(+)
  , (-)
  , (*) ::
    forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.* bv2

negate, abs, signum :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
negate (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.negate bv
abs (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.abs bv
signum (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.signum bv

-- Eq and Ord
eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Bool
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2
lt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.lt s1 s2
le (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.le s1 s2
gt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.gt s1 s2
ge (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.ge s1 s2

-- Bounded
minBound, maxBound :: forall n. (KnownNat n) => Unsigned n
minBound = as @(Unsigned n) $ BV.minBound
maxBound = as @(Unsigned n) $ BV.maxBound

-- Bits
and, or, xor :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
and (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.and` bv2
or (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.or` bv2
xor (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.xor` bv2

complement :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
complement (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.complement bv

zeroBits :: forall n. (KnownNat n) => Unsigned n
zeroBits = as @(Unsigned n) BV.zeroBits

bit :: forall n. (KnownNat n) => Int -> Unsigned n
bit i = as @(Unsigned n) $ BV.bit i

testBit :: forall n. (KnownNat n) => Unsigned n -> Int -> Bool
testBit (as @(BitVector n) -> bv) = BV.testBit bv

bitSizeMaybe :: forall n. (KnownNat n) => Unsigned n -> Maybe Int
bitSizeMaybe (as @(BitVector n) -> bv) = BV.bitSizeMaybe bv

bitSize :: forall n. (KnownNat n) => Unsigned n -> Int
bitSize (as @(BitVector n) -> bv) = BV.bitSize bv

isSigned :: (KnownNat n) => Unsigned n -> Bool
isSigned _ = False

setBit
  , clearBit
  , complementBit ::
    forall n. (KnownNat n) => Unsigned n -> Int -> Unsigned n
setBit (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.setBit bv i
clearBit (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.clearBit bv i
complementBit (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.complementBit bv i

shiftL
  , shiftR
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => Unsigned n -> Int -> Unsigned n
shiftL (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.shiftL bv i
shiftR (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.shiftR bv i
rotateL (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.rotateL bv i
rotateR (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.rotateR bv i

popCount :: forall n. (KnownNat n) => Unsigned n -> Int
popCount (as @(BitVector n) -> bv) = BV.popCount bv
