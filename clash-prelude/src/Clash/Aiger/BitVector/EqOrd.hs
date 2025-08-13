{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector.EqOrd where

import GHC.TypeLits (KnownNat)

import Clash.Aiger.BitVector.Bits (reduceAnd)
import Clash.Aiger.Util (maybeDestructBV2, zipWithBV, as)
import  Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.Bit.EqOrd as Bit

neq, eq :: (KnownNat n) => BitVector n -> BitVector n -> Bool
neq bv1 bv2 = as @Bool $ Base.complement $ eq# bv1 bv2
eq bv1 bv2 = as @Bool $ eq# bv1 bv2

eq# :: (KnownNat n) => BitVector n -> BitVector n -> Bit
eq# bv1 bv2 = reduceAnd (zipWithBV (Bit.eq#) bv1 bv2)

-- Ord BitVector
lt
  , le
  , gt
  , ge ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> Bool
lt bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then lt bs1 bs2 else Bit.lt b1 b2
le bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then le bs1 bs2 else Bit.lt b1 b2
gt bv1 bv2 = maybeDestructBV2 go False bv1 bv2
 where
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then gt bs1 bs2 else Bit.gt b1 b2
ge bv1 bv2 = maybeDestructBV2 go True bv1 bv2
 where
  go b1 b2 bs1 bs2 = if Bit.eq b1 b2 then ge bs1 bs2 else Bit.gt b1 b2
