{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Bit.EqOrd where

import Prelude hiding (and)

import Clash.Aiger.Util (as)
import  Clash.Sized.Internal.BitVector (Bit)

import qualified Clash.Aiger.Base as Base

toBool :: Bit -> Bool
toBool b = as @Bool b

neq, eq, lt, le, gt, ge :: Bit -> Bit -> Bool
neq b1 b2 = toBool $ neq# b1 b2
eq b1 b2 = toBool $ eq# b1 b2
lt b1 b2 = toBool $ lt# b1 b2
ge b1 b2 = toBool $ ge# b1 b2
gt b1 b2 = toBool $ gt# b1 b2
le b1 b2 = toBool $ le# b1 b2

eq#, neq#, lt#, gt#, le#, ge# :: Bit -> Bit -> Bit
neq# b1 b2 = Base.xor b1 b2
eq# b1 b2 = Base.complement $ neq# b1 b2
lt# b1 b2 = b2 `Base.and` (Base.complement b1)
gt# b1 b2 = b1 `Base.and` (Base.complement b2)
le# b1 b2 = Base.complement $ gt# b1 b2
ge# b1 b2 = Base.complement $ lt# b1 b2
