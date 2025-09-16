module Clash.Aiger.IntHash where
import GHC.Base (Int16#, Int#)

(+#), (-#), (*#) :: Int# -> Int# -> Int#
eq#
  , neq#
  , lt#
  , le#
  , gt#
  , ge# ::
    Int# -> Int# -> Bool
intToInt16# :: Int# -> Int16#
