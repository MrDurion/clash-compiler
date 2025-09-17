{-# LANGUAGE RoleAnnotations #-}

module Clash.Sized.Internal.Signed where

import Data.Kind (Type)
import GHC.TypeLits (Nat)

type role Signed nominal

data Signed :: Nat -> Type
