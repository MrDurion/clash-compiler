module Clash.Aiger.Unsigned where

import GHC.TypeLits (KnownNat)

import {-# SOURCE #-} Clash.Sized.Internal.Unsigned

import qualified Clash.Sized.Internal.BitVector as BV

xorAIGER# :: Unsigned n -> Unsigned n -> Unsigned n
xorAIGER# v1 v2 = unpack# (pack# v1 `BV.xor#` pack# v2)
