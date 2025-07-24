{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Aiger.Prim where

import GHC.Base (Int#, Int8#)
import GHC.Int (Int (I#), Int8 (I8#))

import qualified GHC.Prim
import qualified Language.Haskell.TH.Syntax as TH

import Clash.Annotations.AigerSubstitution (
  AigerSubstitution (AigerSubstitution),
 )
import Clash.Class.BitPack (BitPack (..), bitCoerce)
import Clash.Class.Resize (resize)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.BitVector as BV

ghcPrimAigerSubstitutions :: [(TH.Name, AigerSubstitution)]
ghcPrimAigerSubstitutions =
  [ ('GHC.Prim.timesInt8#, AigerSubstitution ('myTimesInt8#))
  , ('GHC.Prim.plusInt8#, AigerSubstitution ('myPlusInt8#))
  , ('GHC.Prim.subInt8#, AigerSubstitution ('mySubInt8#))
  , ('GHC.Prim.int8ToInt#, AigerSubstitution ('int8ToInt#))
  -- ,(('GHC.Prim.timesInt16#), AigerSubstitution ('AIGER_PRIM.timesInt16#))
  -- ,(('GHC.Prim.timesInt32#), AigerSubstitution ('AIGER_PRIM.timesInt32#))
  -- ,(('GHC.Prim.timesInt64#), AigerSubstitution ('AIGER_PRIM.timesInt64#))
  ]

getI8# :: Int8 -> Int8#
getI8# a = case a of I8# b -> b

getI# :: Int -> Int#
getI# a = case a of I# b -> b

int8ToSigned8 :: Int8 -> Signed 8
int8ToSigned8 = bitCoerce
signed64ToInt :: Signed 64 -> Int
signed64ToInt = bitCoerce

int8ToInt# :: Int8# -> Int#
int8ToInt# a = getI# (signed64ToInt (resize (int8ToSigned8 (I8# a))))

times#, plus#, sub#, quot#, rem# :: forall a. (BitPack a) => a -> a -> a
times# a b = unpack $ (pack a) BV.*# (pack b)
plus# a b = unpack $ (pack a) BV.+# (pack b)
sub# a b = unpack $ (pack a) BV.-# (pack b)
quot# a b = unpack $ (pack a) BV./# (pack b)
rem# a b = unpack $ (pack a) BV.%# (pack b)

negate# :: forall a. (BitPack a) => a -> a
negate# = unpack . BV.negate# . pack

myTimesInt8#, myPlusInt8#, mySubInt8# :: Int8# -> Int8# -> Int8#
myTimesInt8# a b = case times# (I8# a) (I8# b) of I8# c -> c
myPlusInt8# a b = case plus# (I8# a) (I8# b) of I8# c -> c
mySubInt8# a b = case sub# (I8# a) (I8# b) of I8# c -> c

-- gt#, lt#, eq#, ge#, le#, ne# :: forall a. (BitPack a) => a -> a -> Int#
-- gt# a b = unpack $ BV.gt# (pack a) (pack b)
-- lt# a b = unpack $ BV.lt# (pack a) (pack b)
-- eq# a b = unpack $ BV.eq# (pack a) (pack b)
-- ge# a b = unpack $ BV.ge# (pack a) (pack b)
-- le# a b = unpack $ BV.le# (pack a) (pack b)
-- ne# a b = unpack $ BV.ne# (pack a) (pack b)
