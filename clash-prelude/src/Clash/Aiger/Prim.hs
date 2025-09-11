{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Prim where

import Data.Int (Int16)
import GHC.Base (Int (I#), Int#, Int16#)
import GHC.Int (Int16 (I16#))

import qualified GHC.Prim
import qualified Language.Haskell.TH.Syntax as TH

import Clash.Aiger.Base (as)
import Clash.Annotations.AigerSubstitution (
  AigerSubstitution (AigerSubstitution),
 )
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Int as Int
import qualified Clash.Aiger.Signed as S

ghcPrimAigerSubstitutions :: [(TH.Name, AigerSubstitution)]
ghcPrimAigerSubstitutions =
  [ ('(GHC.Prim.+#), AigerSubstitution ('(+#)))
  , ('(GHC.Prim.-#), AigerSubstitution ('(-#)))
  , ('(GHC.Prim.*#), AigerSubstitution ('(*#)))
  , ('(GHC.Prim.==#), AigerSubstitution ('eq#))
  , ('(GHC.Prim./=#), AigerSubstitution ('neq#))
  , ('(GHC.Prim.<#), AigerSubstitution ('lt#))
  , ('(GHC.Prim.>#), AigerSubstitution ('gt#))
  , ('(GHC.Prim.<=#), AigerSubstitution ('le#))
  , ('(GHC.Prim.>=#), AigerSubstitution ('ge#))
  , ('GHC.Prim.intToInt16#, AigerSubstitution ('intToInt16#))
  ]

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
