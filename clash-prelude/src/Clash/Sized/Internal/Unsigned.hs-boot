{-# LANGUAGE RoleAnnotations #-}

module Clash.Sized.Internal.Unsigned where

import Data.Kind (Type)
import GHC.TypeLits (Nat)

type role Unsigned nominal

data Unsigned :: Nat -> Type
