module Clash.Aiger.Bool.Num where

import Clash.Sized.Internal.BitVector (Bit)

import qualified Clash.Aiger.Bit.Num as Bit
import Clash.Aiger.Util (as)

toBit :: Bool -> Bit
toBit b = as @Bit @Bool b

asBool :: Bit -> Bool
asBool b = as @Bool @Bit b

(+), (-), (*) :: Bool -> Bool -> Bool
negate, abs, signum :: Bool -> Bool
(+) (toBit -> b1) (toBit -> b2) = asBool $ (Bit.+) b1 b2
(-) (toBit -> b1) (toBit -> b2) = asBool $ (Bit.-) b1 b2
(*) (toBit -> b1) (toBit -> b2) = asBool $ (Bit.*) b1 b2
negate (toBit -> b) = asBool $ Bit.negate b
abs (toBit -> b) = asBool $ Bit.abs b
signum (toBit -> b) = asBool $ Bit.signum b
