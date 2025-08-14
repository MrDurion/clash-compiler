module Clash.Aiger.Unsigned.Bits where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Aiger.BitVector.Bits as BV

and, or, xor :: forall n. (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
and (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.and` bv2
or (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.or` bv2
xor (as @(BitVector n) -> bv1) (as @(BitVector n) -> bv2) = as @(Unsigned n) $ bv1 `BV.xor` bv2

complement :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
complement (as @(BitVector n) -> bv) = as @(Unsigned n) $ BV.complement bv
