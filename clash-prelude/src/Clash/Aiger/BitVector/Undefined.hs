module Clash.Aiger.BitVector.Undefined where
import GHC.TypeLits (KnownNat)


import  Clash.Sized.Internal.BitVector (BitVector)
import Clash.Aiger.Util (repeatBV)
import Clash.Aiger.Base (undefined##)

undefined# :: (KnownNat n) => BitVector n
undefined# = repeatBV (undefined##)
