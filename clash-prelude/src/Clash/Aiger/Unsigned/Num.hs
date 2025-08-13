module Clash.Aiger.Unsigned.Num where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Util (as)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Aiger.BitVector.Num as BV

(+)
  , (-)
  , (*) ::
    forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
(+) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.+ bv2
(-) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.- bv2
(*) (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 BV.* bv2

negate :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
negate (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.negate bv
