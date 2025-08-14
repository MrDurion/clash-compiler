{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Prim where

import qualified GHC.Prim
import qualified Language.Haskell.TH.Syntax as TH

import Clash.Annotations.AigerSubstitution (
  AigerSubstitution (AigerSubstitution),
 )

import qualified Clash.Aiger.Int as Int

ghcPrimAigerSubstitutions :: [(TH.Name, AigerSubstitution)]
ghcPrimAigerSubstitutions =
  [ ('(GHC.Prim.+#), AigerSubstitution ('(Int.+)))
  , ('(GHC.Prim.-#), AigerSubstitution ('(Int.-)))
  , ('(GHC.Prim.*#), AigerSubstitution ('(Int.*)))
  ]
