{-# LANGUAGE TemplateHaskell #-}

module Clash.Annotations.AigerSubstitution where

import Data.Data (Data)
import Language.Haskell.TH (Name)

data AigerSubstitution = AigerSubstitution Name deriving (Data, Show)
data PrimitiveAigerSubstitution = PrimitiveAigerSubstitution String
  deriving (Data, Show)

primitiveAigerSubstitution :: String -> PrimitiveAigerSubstitution
primitiveAigerSubstitution n = PrimitiveAigerSubstitution n

aigerSubstitution :: Name -> AigerSubstitution
aigerSubstitution n = AigerSubstitution n

aigerModuleNames :: [String]
aigerModuleNames =
  [ "Clash.Aiger.Bit"
  , "Clash.Aiger.BitVector"
  , "Clash.Aiger.Bool"
  , "Clash.Aiger.Unsigned"
  , "Clash.Aiger.Signed"
  , "Clash.Aiger.Int"
  ]
