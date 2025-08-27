module Clash.Aiger.Int where

import GHC.Base (Int (I#), Int#)
import Prelude hiding ((*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Signed as S

-- Util
asSigned :: Int -> Signed 64
asSigned i = as @(Signed 64) i

fromSigned :: Signed 64 -> Int
fromSigned s = as @(Int) s

asSigned# :: Int# -> Signed 64
asSigned# i = as @(Signed 64) (I# i)

fromSigned# :: Signed 64 -> Int#
fromSigned# s = case as @(Int) s of I# d -> d

-- Num
(+#), (-#), (*#) :: Int# -> Int# -> Int#
(+#) (asSigned# -> s1) (asSigned# -> s2) = fromSigned# $ s1 S.+ s2
(-#) (asSigned# -> s1) (asSigned# -> s2) = fromSigned# $ s1 S.- s2
(*#) (asSigned# -> s1) (asSigned# -> s2) = fromSigned# $ s1 S.* s2

-- Eq and Ord
eq#
  , neq#
  , lt#
  , le#
  , gt#
  , ge# ::
    Int# -> Int# -> Bool
eq# (asSigned# -> s1) (asSigned# -> s2) = S.eq s1 s2
neq# (asSigned# -> s1) (asSigned# -> s2) = S.neq s1 s2
lt# (asSigned# -> s1) (asSigned# -> s2) = S.lt s1 s2
le# (asSigned# -> s1) (asSigned# -> s2) = S.le s1 s2
gt# (asSigned# -> s1) (asSigned# -> s2) = S.gt s1 s2
ge# (asSigned# -> s1) (asSigned# -> s2) = S.ge s1 s2

eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    Int -> Int -> Bool
eq (asSigned -> s1) (asSigned -> s2) = S.eq s1 s2
neq (asSigned -> s1) (asSigned -> s2) = S.neq s1 s2
lt (asSigned -> s1) (asSigned -> s2) = S.lt s1 s2
le (asSigned -> s1) (asSigned -> s2) = S.le s1 s2
gt (asSigned -> s1) (asSigned -> s2) = S.gt s1 s2
ge (asSigned -> s1) (asSigned -> s2) = S.ge s1 s2

(+), (-), (*) :: Int -> Int -> Int
(+) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.+ s2
(-) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.- s2
(*) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.* s2
