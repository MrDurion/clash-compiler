{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.BitVector.Num where

import GHC.TypeLits (KnownNat)
import Prelude hiding (negate, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Aiger.Util (
  all0BV,
  mapBV,
  maybeDestructBV,
  maybeDestructBV2,
  maybeLDestructBV,
 )
import Clash.Sized.Internal.BitVector (Bit, BitVector)

import qualified Clash.Aiger.Base as Base
import qualified Clash.Aiger.BitVector.Bits as BitVector

-- #####
-- BitVector NumWithResize or something already, not in scope tho
--
-- plus# ::
--   forall m n.
--   (KnownNat m, KnownNat n, m <= Max m n, n <= Max m n) =>
--   BitVector m -> BitVector n -> BitVector (Max m n + 1)
-- plus# a b = a1 +# b1
--  where
--   a1 = growBV a
--   b1 = growBV b
--
-- minus# ::
--   forall m n.
--   (KnownNat m, KnownNat n, m <= Max m n, n <= Max m n) =>
--   BitVector m ->
--   BitVector n ->
--   BitVector
--     (Max m n + 1)
-- minus# a b = a1 -# b1
--  where
--   a1 = growBV a
--   b1 = growBV b
--
-- times# ::
--   forall m n.
--   (KnownNat m, KnownNat n) => BitVector m -> BitVector n -> BitVector (m + n)
-- times# a b = a1 *# b1
--  where
--   a1 = growBV a
--   b1 = growBV b

-- Num BitVector

(-)
  , (+)
  , (*) ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
(+) a b = fst $ adder a b
(-) a b = a + (negate b)
(*) a b = shiftAdd b
 where
  shiftAdd ::
    forall m. (KnownNat m) => BitVector m -> BitVector n
  shiftAdd bv = maybeLDestructBV go def bv
   where
    go bb c = (BitVector.shiftlBV (shiftAdd bb)) + (andA c)
    def = all0BV
  andA i = mapBV (`Base.and` i) a

negate :: forall n. (KnownNat n) => BitVector n -> BitVector n
negate bv = fst $ negateBV bv

-- Util

negateBV :: forall n. (KnownNat n) => BitVector n -> (BitVector n, Bit)
negateBV bv = maybeDestructBV go (all0BV, Base.high) bv
 where
  go b bs =
    let
      (r, c) = negateBV bs
      (bitr, bitc) = halfAdder (Base.complement b) c
     in
      ((as @(BitVector 1) bitr) Base.++# r, bitc)

adder ::
  forall n. (KnownNat n) => BitVector n -> BitVector n -> (BitVector n, Bit)
adder bv1 bv2 = maybeDestructBV2 go (all0BV, Base.low) bv1 bv2
 where
  go b1 b2 bs1 bs2 =
    let
      (r, c) = adder bs1 bs2
      (bitr, bitc) = fullAdder b1 b2 c
     in
      ((as @(BitVector 1) bitr) Base.++# r, bitc)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b c = (r2, c_out)
 where
  (r1, c1) = halfAdder a b
  (r2, c2) = halfAdder c r1
  c_out = Base.or c1 c2

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder b1 b2 = (r, c)
 where
  c = b1 `Base.and` b2
  r = b1 `Base.xor` b2
