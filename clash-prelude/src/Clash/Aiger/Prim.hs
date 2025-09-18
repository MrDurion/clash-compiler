{-# LANGUAGE TemplateHaskell #-}

module Clash.Aiger.Prim where

import qualified GHC.Classes
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
  , ('GHC.Classes.eqInt, AigerSubstitution ('IntHash.eq#))
  , ('GHC.Classes.neInt, AigerSubstitution ('IntHash.neq#))
  , ('GHC.Classes.ltInt, AigerSubstitution ('IntHash.lt#))
  , ('GHC.Classes.gtInt, AigerSubstitution ('IntHash.gt#))
  , ('GHC.Classes.geInt, AigerSubstitution ('IntHash.ge#))
  , -- The followingone can not be added yet, since it causes infinite recursion during compilation
    ('GHC.Classes.leInt, AigerSubstitution ('IntHash.le#))
  , ('GHC.Prim.intToInt16#, AigerSubstitution ('IntHash.intToInt16#))
  ]
