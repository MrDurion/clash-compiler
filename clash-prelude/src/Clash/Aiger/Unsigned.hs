module Clash.Aiger.Unsigned where

import GHC.TypeLits (KnownNat)

import {-# SOURCE #-} qualified Clash.Sized.Internal.Unsigned as U
import {-# SOURCE #-} Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Sized.Internal.BitVector as BV

xor# :: KnownNat n => Unsigned n -> Unsigned n -> Unsigned n
xor# v1 v2 = U.unpack# (U.pack# v1 `BV.xor#` U.pack# v2)
