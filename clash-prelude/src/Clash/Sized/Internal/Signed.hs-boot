{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

module Clash.Sized.Internal.Signed where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat)

import Clash.Sized.Internal.BitVector

type role Signed nominal

data Signed :: Nat -> Type
pack# :: forall n. (KnownNat n) => Signed n -> BitVector n
unpack# :: forall n. (KnownNat n) => BitVector n -> Signed n
