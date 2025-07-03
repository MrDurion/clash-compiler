module Clash.Aiger.Unsigned where

import GHC.TypeLits (KnownNat)

import {-# SOURCE #-} Clash.Sized.Internal.Unsigned (Unsigned)

import qualified Clash.Sized.Internal.BitVector as BV
import {-# SOURCE #-} qualified Clash.Sized.Internal.Unsigned as U

xor# :: (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
xor# v1 v2 = U.unpack# (U.pack# v1 `BV.xor#` U.pack# v2)

or# :: (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
or# v1 v2 = U.unpack# (U.pack# v1 `BV.or#` U.pack# v2)

and# :: (KnownNat n) => Unsigned n -> Unsigned n -> Unsigned n
and# v1 v2 = U.unpack# (U.pack# v1 `BV.and#` U.pack# v2)

complement# :: forall n. (KnownNat n) => Unsigned n -> Unsigned n
complement# v1 = U.unpack# (BV.complement# $ U.pack# v1)
