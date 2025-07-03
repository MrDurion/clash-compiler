{-# LANGUAGE OverloadedStrings #-}

module Clash.Netlist.Id.AIGER where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Data.Text as Text

import Clash.Netlist.Types (IdentifierType (..))

-- TODO what does this do?
unextend :: Text -> Text
unextend =
  Text.strip
    . (\t -> fromMaybe t (Text.stripPrefix "\\" t))
    . Text.strip

-- function to create a text version of an Identifier
toText :: IdentifierType -> Text -> Text
toText Basic t = t
toText Extended t = t

-- impl from VERILOG:
-- toText Basic t = t
-- toText Extended t = "\\" <> t <> " "

-- TODO
parseBasic :: Text -> Bool
parseBasic = const False

-- TODO
parseExtended :: Text -> Bool
parseExtended = const False

-- TODO
toBasic :: Text -> Text
toBasic = id
