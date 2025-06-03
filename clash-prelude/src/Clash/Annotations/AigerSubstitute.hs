{-# LANGUAGE TemplateHaskell #-}

module Clash.Annotations.AigerSubstitute where

import Data.Data (Data)
import Language.Haskell.TH (Name)

data AigerSubstitution = AigerSubstitution Name deriving (Data)

aigerSubstitute :: Name -> AigerSubstitution
aigerSubstitute n = AigerSubstitution n
