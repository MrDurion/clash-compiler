{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Clash.Tests.Aiger.TH where

import Control.Exception (SomeException, evaluate, try)
import GHC.IO (unsafePerformIO)
import Test.Tasty

import qualified Test.Tasty.QuickCheck as QC

import Clash.Sized.Internal.BitVector (Bit (..))

instance QC.Arbitrary Bit where
  arbitrary :: QC.Gen Bit
  arbitrary = QC.elements [(Bit 0 0), (Bit 0 1)]

class TestEq f where
  eqProp :: f -> f -> QC.Property

instance {-# OVERLAPS #-} (Eq a, Show a) => TestEq a where
  eqProp x y = QC.counterexample ("Expected " ++ show y ++ " but got " ++ show x) (x == y)

instance (TestEq b) => TestEq (Int -> b) where
  eqProp f g = QC.forAll (QC.arbitrary `QC.suchThat` (> 0)) $ \x -> case safeEval (g x) of
    Nothing -> QC.discard
    Just b -> eqProp (f x) b

instance {-# OVERLAPS #-} (QC.Arbitrary a, Show a, TestEq b) => TestEq (a -> b) where
  eqProp f g = QC.property $ \x -> case safeEval (g x) of
    Nothing -> QC.discard
    Just b -> eqProp (f x) b

eqTest :: (TestEq f) => String -> f -> f -> TestTree
eqTest name f g = QC.testProperty name (eqProp f g)

safeEval :: forall a. a -> Maybe a
safeEval x = unsafePerformIO $ do
  r <- try (evaluate x) :: IO (Either SomeException a)
  pure (either (const Nothing) Just r)
