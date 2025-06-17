{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

{- |
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Clash.Sized.Internal.BitVector where

import GHC.TypeLits (KnownNat, Nat, Natural, type (+))

type role BitVector nominal

data BitVector (n :: Nat)
  = BV
  { unsafeMask :: !Natural
  , unsafeToNatural :: !Natural
  }
data Bit

complement## :: Bit -> Bit
undefError :: (KnownNat n) => String -> [BitVector n] -> a
split# ::
  forall n m.
  (KnownNat n) =>
  BitVector (m + n) ->
  (BitVector m, BitVector n)
pack# :: Bit -> BitVector 1
eq## :: Bit -> Bit -> Bool
eq# :: (KnownNat n) => BitVector n -> BitVector n -> Bool
unpack# :: BitVector 1 -> Bit
and##, or##, xor## :: Bit -> Bit -> Bit
lt#, ge#, gt#, le# :: (KnownNat n) => BitVector n -> BitVector n -> Bool
lt##, ge##, gt##, le## :: Bit -> Bit -> Bool
low :: Bit
high :: Bit
(++#) :: (KnownNat m) => BitVector n -> BitVector m -> BitVector (n + m)
(+#)
  , (-#)
  , (*#) ::
    forall n. (KnownNat n) => BitVector n -> BitVector n -> BitVector n
