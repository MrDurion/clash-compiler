module Clash.Aiger.Bit.Num where

import Clash.Sized.Internal.BitVector (Bit)

import qualified Clash.Aiger.Base as Base

(+), (-), (*) :: Bit -> Bit -> Bit
negate, abs, signum :: Bit -> Bit
(+) = Base.xor
(-) = Base.xor
(*) = Base.and
negate = Base.complement
abs = id
signum = id
