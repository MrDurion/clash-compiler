module Clash.Aiger.Int.Num where

import GHC.Base (Int (I#), Int#)

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Signed.Num as S

asSigned :: Int# -> Signed 64
asSigned i = as @(Signed 64) (I# i)

fromSigned :: Signed 64 -> Int#
fromSigned s = case as @(Int) s of I# d -> d

(+), (-), (*) :: Int# -> Int# -> Int#
(+) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.+ s2
(-) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.- s2
(*) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.* s2
