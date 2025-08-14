module Clash.Aiger.Unsigned.EqOrd where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Aiger.BitVector.EqOrd as BV

eq, neq :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Bool
eq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.eq s1 s2
neq (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.neq s1 s2

lt, le, gt, ge :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Bool
lt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.lt s1 s2
le (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.le s1 s2
gt (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.gt s1 s2
ge (as @(BitVector n) -> s1) (as @(BitVector n) -> s2) = BV.ge s1 s2
