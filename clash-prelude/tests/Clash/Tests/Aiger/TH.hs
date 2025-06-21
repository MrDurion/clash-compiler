{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Clash.Tests.Aiger.TH where

import GHC.TypeLits (KnownNat)
import Language.Haskell.TH

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Clash.Sized.Internal.BitVector (BitVector)

import qualified Test.Tasty.Hedgehog.Extra as H

genTests :: Name -> Name -> Q Exp
genTests fName bvName = do
  exps <- mapM (genTHTest fName bvName) [0, 1, 2, 16, 127, 128]
  return $ ListE exps

-- Helper to build: f (Proxy :: Proxy N)
genTHTest :: Name -> Name -> Int -> Q Exp
genTHTest fName bvName n = do
  let t = LitT (NumTyLit (fromIntegral n))
  let genCompareTestsTyped = (AppTypeE (VarE 'genCompareTests) t)
  let applied = AppE (AppE (genCompareTestsTyped) (VarE fName)) (VarE bvName)
  htest <- [e|H.testPropertyXXX ("bitvector " ++ show (n :: Int))|]
  pure $ AppE htest applied

-- Genable
class (Show b, Eq b) => Genable b where
  genThing :: H.Gen (b)

instance Genable Int where
  genThing = Gen.integral (Range.linear 0 512)

instance (KnownNat n) => Genable (BitVector n) where
  genThing = Gen.integral (Range.linear 0 265)

-- CompareTestable
class CompareTestable a where
  genTest :: a -> a -> H.PropertyT IO ()

instance {-# OVERLAPS #-} forall a. (Show a, Eq a) => CompareTestable a where
  genTest a b = do
    a H.=== b

instance {-# OVERLAPS #-} (Genable b, CompareTestable a) => CompareTestable (b -> a) where
  genTest f1 f2 = do
    (c :: b) <- H.forAll genThing
    genTest (f1 c) (f2 c)

genCompareTests ::
  forall n q a.
  (Genable (q n), KnownNat n, CompareTestable a) =>
  (q n -> a) ->
  (q n -> a) ->
  H.Property
genCompareTests aigerF bvF = H.property $ do
  genTest aigerF bvF
