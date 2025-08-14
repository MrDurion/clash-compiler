module Clash.Aiger.Int.EqOrd where

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Signed.EqOrd as S

eqInt
  , neqInt
  , ltInt
  , leInt
  , gtInt
  , geInt ::
    Int -> Int -> Bool
eqInt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.eq s1 s2
neqInt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.neq s1 s2
ltInt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.lt s1 s2
leInt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.le s1 s2
gtInt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.gt s1 s2
geInt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.ge s1 s2
