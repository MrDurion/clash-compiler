{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Signed where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import Prelude hiding (and, negate, or, truncate, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (all0BV, all1BV, comp, flipFirstBit)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.BitVector as BV

-- BitPack

unpack :: forall n. (KnownNat n) => BitVector n -> Signed n
unpack = as @(Signed n) @(BitVector n)

pack :: forall n. (KnownNat n) => Signed n -> BitVector n
pack = as @(BitVector n) @(Signed n)

-- Resize
resize :: forall n m. (KnownNat n, KnownNat m) => Signed n -> Signed m
resize (as @(BitVector n) -> bv) = as @(Signed m) $ go bv
 where
  go :: BitVector n -> BitVector m
  go = comp @n @m truncate grow

  truncate ::
    (m <= n) => BitVector (m + (n - m)) -> BitVector m
  truncate = BV.truncateB

  grow :: (n <= m) => BitVector n -> BitVector m
  grow = BV.signExtend

-- Num
(+), (-), (*) :: forall n. (KnownNat n) => Signed n -> Signed n -> Signed n
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.* bv2

negate :: forall n. (KnownNat n) => Signed n -> Signed n
negate (as @(BitVector n) -> bv) = as @(Signed n) $ BV.negate bv

-- Eq and Ord
eq, neq :: forall n. (KnownNat n) => Signed n -> Signed n -> Bool
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2

lt, le, gt, ge :: forall n. (KnownNat n) => Signed n -> Signed n -> Bool
lt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.lt (flipFirstBit bv1) (flipFirstBit bv2)
le (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.le (flipFirstBit bv1) (flipFirstBit bv2)
gt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.gt (flipFirstBit bv1) (flipFirstBit bv2)
ge (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.ge (flipFirstBit bv1) (flipFirstBit bv2)

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
