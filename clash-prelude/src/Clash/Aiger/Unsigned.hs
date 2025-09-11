{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Unsigned
where

import GHC.TypeLits (KnownNat, type (+), type (<=), type (-))
import Prelude hiding (and, negate, or, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (all0BV, all1BV)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Aiger.BitVector as BV

unpack :: forall n. (KnownNat n) => BitVector n -> Unsigned n
pack :: forall n. (KnownNat n) => Unsigned n -> BitVector n
resize :: forall n m. (KnownNat n, KnownNat m) => Unsigned n -> Unsigned m
truncateB ::
  forall a b. (KnownNat a, KnownNat b) => Unsigned (a + b) -> Unsigned a
zeroExtend ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => Unsigned m -> Unsigned n
signExtend ::
  forall m n. (KnownNat n, KnownNat m, m <= n) => Unsigned m -> Unsigned n
(+)
  , (-)
  , (*) ::
    forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
negate, abs, signum :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Bool
minBound, maxBound :: forall n. (KnownNat n) => Unsigned n
and, or, xor :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
complement :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
zeroBits :: forall n. (KnownNat n) => Unsigned n
bit :: forall n. (KnownNat n) => Int -> Unsigned n
testBit :: forall n. (KnownNat n) => Unsigned n -> Int -> Bool
bitSizeMaybe :: forall n. (KnownNat n) => Unsigned n -> Maybe Int
bitSize, popCount :: forall n. (KnownNat n) => Unsigned n -> Int
isSigned :: (KnownNat n) => Unsigned n -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    forall n. (KnownNat n) => Unsigned n -> Int -> Unsigned n
undefined# :: forall n. (KnownNat n) => Unsigned n
-- Implementations

-- BitPack
unpack = as @(Unsigned n) @(BitVector n)
pack = as @(BitVector n) @(Unsigned n)

-- Resize
resize (as @(BitVector n) -> bv) = as @(Unsigned m) $ BV.resize bv
truncateB (as @(BitVector (a + b)) -> bv) = as @(Unsigned a) $ BV.truncateB bv
signExtend (as @(BitVector m) -> bv) = as @(Unsigned n) $ BV.signExtend @m @(n - m) bv
zeroExtend (as @(BitVector m) -> bv) = as @(Unsigned n) $ BV.zeroExtend @m @(n - m) bv

-- Undefined
undefined# = as @(Unsigned n) BV.undefined#

-- Num
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.* bv2

negate (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.negate bv
abs (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.abs bv
signum (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.signum bv

-- Eq and Ord
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2

lt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.lt s1 s2
le (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.le s1 s2
gt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.gt s1 s2
ge (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.ge s1 s2

-- Bounded
minBound = as @(Unsigned n) $ all0BV
maxBound = as @(Unsigned n) $ all1BV

-- Bits
and (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.and` bv2
or (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.or` bv2
xor (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.xor` bv2
complement (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.complement bv
zeroBits = as @(Unsigned n) BV.zeroBits
bit i = as @(Unsigned n) $ BV.bit i
testBit (as @(BitVector n) -> bv) = BV.testBit bv
bitSizeMaybe (as @(BitVector n) -> bv) = BV.bitSizeMaybe bv
bitSize (as @(BitVector n) -> bv) = BV.bitSize bv
isSigned _ = False
setBit (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.setBit bv i
clearBit (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.clearBit bv i
complementBit (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.complementBit bv i
shiftL (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.shiftL bv i
shiftR (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.shiftR bv i
rotateL (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.rotateL bv i
rotateR (as @(BitVector n) -> bv) i = as @(Unsigned n) $ BV.rotateR bv i
popCount (as @(BitVector n) -> bv) = BV.popCount bv
