{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

module Clash.Sized.Internal.Unsigned where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat)

import Clash.Sized.Internal.BitVector (BitVector)

type role Unsigned nominal

data Unsigned :: Nat -> Type
pack# :: Unsigned n -> BitVector n
unpack# :: (KnownNat n) => BitVector n -> Unsigned n
