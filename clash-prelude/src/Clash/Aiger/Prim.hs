{-# LANGUAGE TemplateHaskell #-}

module Clash.Aiger.Prim where

import qualified GHC.Prim
import qualified Language.Haskell.TH.Syntax as TH

import Clash.Annotations.AigerSubstitution (
  AigerSubstitution (AigerSubstitution),
 )

import qualified Clash.Aiger.IntHash as IntHash

ghcPrimAigerSubstitutions :: [(TH.Name, AigerSubstitution)]
ghcPrimAigerSubstitutions =
  [ ('(GHC.Prim.+#), AigerSubstitution ('(IntHash.+#)))
  , ('(GHC.Prim.-#), AigerSubstitution ('(IntHash.-#)))
  , ('(GHC.Prim.*#), AigerSubstitution ('(IntHash.*#)))
  , ('(GHC.Prim.==#), AigerSubstitution ('IntHash.eq#))
  , ('(GHC.Prim./=#), AigerSubstitution ('IntHash.neq#))
  , ('(GHC.Prim.<#), AigerSubstitution ('IntHash.lt#))
  , ('(GHC.Prim.>#), AigerSubstitution ('IntHash.gt#))
  , ('(GHC.Prim.>=#), AigerSubstitution ('IntHash.ge#))
  , -- The followingone can not be added yet, since it causes infinite recursion during compilation
    -- , ('(GHC.Prim.<=#), AigerSubstitution ('IntHash.le#))
    ('GHC.Prim.intToInt16#, AigerSubstitution ('IntHash.intToInt16#))
  ]
