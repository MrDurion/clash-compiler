module Clash.Aiger.IntHash where

import Clash.Aiger.Base (as)

import qualified Clash.Aiger.Int as Int
import GHC.Int (Int16 (I16#))
import GHC.Base (Int16#, Int#, Int (I#))
import Clash.Sized.Internal.Signed (Signed)
import qualified Clash.Aiger.Signed as S

-- Util
asInt :: Int# -> Int
asInt i = I# i

asInt# :: Int -> Int#
asInt# ii = case ii of (I# i) -> i

asInt16# :: Int16 -> Int16#
asInt16# i16 = case i16 of (I16# i16#) -> i16#

-- Num
(+#), (-#), (*#) :: Int# -> Int# -> Int#
(+#) (asInt -> i1) (asInt -> i2) = asInt# $ (Int.+) i1 i2
(-#) (asInt -> i1) (asInt -> i2) = asInt# $ (Int.-) i1 i2
(*#) (asInt -> i1) (asInt -> i2) = asInt# $ (Int.*) i1 i2

-- Eq and Ord
eq#
  , neq#
  , lt#
  , le#
  , gt#
  , ge# ::
    Int# -> Int# -> Bool
eq# (asInt -> i1) (asInt -> i2) = Int.eq i1 i2
neq# (asInt -> i1) (asInt -> i2) = Int.neq i1 i2
lt# (asInt -> i1) (asInt -> i2) = Int.lt i1 i2
le# (asInt -> i1) (asInt -> i2) = Int.le i1 i2
gt# (asInt -> i1) (asInt -> i2) = Int.gt i1 i2
ge# (asInt -> i1) (asInt -> i2) = Int.ge i1 i2

intToInt16# :: Int# -> Int16#
intToInt16# (asInt -> i) =
  let
    signed64 = as @(Signed 64) i
    signed16 = S.resize signed64
    int16 = as @(Int16) signed16
   in
    asInt16# $ int16
