module Clash.Aiger.Signed.EqOrd where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (flipFirstBit)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.BitVector.EqOrd as BV

eq, neq :: forall n. (KnownNat n) => Signed n -> Signed n -> Bool
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2

lt, le, gt, ge :: forall n. (KnownNat n) => Signed n -> Signed n -> Bool
lt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.lt (flipFirstBit bv1) (flipFirstBit bv2)
le (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.le (flipFirstBit bv1) (flipFirstBit bv2)
gt (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.gt (flipFirstBit bv1) (flipFirstBit bv2)
ge (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = BV.ge (flipFirstBit bv1) (flipFirstBit bv2)
