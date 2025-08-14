module Clash.Aiger.Signed.Num where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.BitVector.Num as BV

(+), (-), (*) :: forall n. (KnownNat n) => Signed n -> Signed n -> Signed n
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as $ bv1 BV.* bv2

negate :: forall n. (KnownNat n) => Signed n -> Signed n
negate (as @(BitVector n) -> bv) = as @(Signed n) $ BV.negate bv
