{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Unsigned
where

import GHC.TypeLits (KnownNat, type (+), type (-), type (<=))
import Prelude hiding (and, negate, or, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (all0BV, all1BV, comp)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Aiger.BitVector as BV

-- Resize
resize :: forall n m. (KnownNat n, KnownNat m) => Unsigned n -> Unsigned m
resize (as @(BitVector n) -> bv) = as @(Unsigned m) $ go bv
 where
  go :: BitVector n -> BitVector m
  go = comp @n @m trunc grow

  trunc ::
    (m <= n) => BitVector (m + (n - m)) -> BitVector m
  trunc = BV.truncateB

  grow :: (n <= m) => BitVector n -> BitVector m
  grow = BV.zeroExtend

-- Num
(+)
  , (-)
  , (*) ::
    forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.* bv2

negate :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
negate (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.negate bv

-- Eq and Ord
eq, neq :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Bool
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2

lt, le, gt, ge :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Bool
lt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.lt s1 s2
le (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.le s1 s2
gt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.gt s1 s2
ge (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.ge s1 s2

-- Bounded
minBound, maxBound :: forall n. (KnownNat n) => Unsigned n
minBound = as @(Unsigned n) $ all0BV
maxBound = as @(Unsigned n) $ all1BV

-- Bits
and, or, xor :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
and (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.and` bv2
or (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.or` bv2
xor (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.xor` bv2

complement :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
complement (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.complement bv
