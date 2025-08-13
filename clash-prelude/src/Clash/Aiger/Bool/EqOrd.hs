module Clash.Aiger.Bool.EqOrd where

import Clash.Aiger.Bit.EqOrd as Bit
import Clash.Aiger.Util (as)
import Clash.Sized.Internal.BitVector (Bit)

toBit :: Bool -> Bit
toBit b = as @Bit @Bool b

eq, neq, lt, le, gt, ge :: Bool -> Bool -> Bool
eq (toBit -> b1) (toBit -> b2) = Bit.eq b1 b2
neq (toBit -> b1) (toBit -> b2) = Bit.neq b1 b2
lt (toBit -> b1) (toBit -> b2) = Bit.lt b1 b2
le (toBit -> b1) (toBit -> b2) = Bit.le b1 b2
gt (toBit -> b1) (toBit -> b2) = Bit.gt b1 b2
ge (toBit -> b1) (toBit -> b2) = Bit.ge b1 b2
