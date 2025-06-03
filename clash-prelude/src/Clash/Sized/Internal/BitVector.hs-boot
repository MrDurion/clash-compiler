{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

{- |
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Clash.Sized.Internal.BitVector where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat, type (+))

type role BitVector nominal
data BitVector :: Nat -> Type
data Bit

undefError :: (KnownNat n) => String -> [BitVector n] -> a
split# ::
  forall n m.
  (KnownNat n) =>
  BitVector (m + n) ->
  (BitVector m, BitVector n)
pack# :: Bit -> BitVector 1
unpack# :: BitVector 1 -> Bit
and##, or##, xor## :: Bit -> Bit -> Bit
(++#) :: (KnownNat m) => BitVector n -> BitVector m -> BitVector (n + m)
