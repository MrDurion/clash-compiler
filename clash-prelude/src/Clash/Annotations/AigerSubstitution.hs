{-# LANGUAGE TemplateHaskell #-}

module Clash.Annotations.AigerSubstitution where

import Data.Data (Data)
import Language.Haskell.TH (Name)

data AigerSubstitution = AigerSubstitution Name deriving (Data, Show)

aigerSubstitution :: Name -> AigerSubstitution
aigerSubstitution n = AigerSubstitution n
