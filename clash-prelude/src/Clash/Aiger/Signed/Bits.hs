module Clash.Aiger.Signed.Bits where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.BitVector.Bits as BV

and, or, xor :: forall n. (KnownNat n) => Signed n -> Signed n -> Signed n
and (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.and bv1 bv2
or (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.or bv1 bv2
xor (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Signed n) $ BV.xor bv1 bv2

complement :: forall n. (KnownNat n) => Signed n -> Signed n
complement (as @(BitVector n) -> bv) = as @(Signed n) $ BV.complement bv
