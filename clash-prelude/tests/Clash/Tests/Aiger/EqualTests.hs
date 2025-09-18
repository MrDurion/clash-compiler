{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Clash.Tests.Aiger.EqualTests where

import Control.Exception (SomeException, evaluate, try)
import Data.Data (Proxy (Proxy))
import GHC.IO (unsafePerformIO)
import GHC.TypeLits (KnownNat, natVal)
import Test.Tasty

import qualified Test.Tasty.QuickCheck as QC

import Clash.Sized.Internal.BitVector (Bit (..))

instance QC.Arbitrary Bit where
  arbitrary :: QC.Gen Bit
  arbitrary = QC.elements [(Bit 0 0), (Bit 0 1)]

class TestEq f where
  eqProp :: Int -> f -> f -> QC.Property

instance {-# OVERLAPS #-} (Eq a, Show a) => TestEq a where
  eqProp _ x y = QC.counterexample ("Expected " ++ show y ++ " but got " ++ show x) (x == y)

instance (TestEq b) => TestEq (Int -> b) where
  eqProp i f g = QC.forAll
    (if i == -1 then QC.arbitrary `QC.suchThat` (>= 0) else QC.elements [0 .. i - 1])
    $ \x -> case safeEval (g x) of
      Nothing -> QC.discard
      Just b -> eqProp i (f x) b

instance {-# OVERLAPS #-} (QC.Arbitrary a, Show a, TestEq b) => TestEq (a -> b) where
  eqProp i f g = QC.property $ \x -> case safeEval (g x) of
    Nothing -> QC.discard
    Just b -> eqProp i (f x) b

eqTestBoundedInt ::
  forall n f. (KnownNat n, TestEq f) => String -> f -> f -> TestTree
eqTestBoundedInt name f g = QC.testProperty name (eqProp (fromInteger (natVal (Proxy @n))) f g)

eqTest :: (TestEq f) => String -> f -> f -> TestTree
eqTest name f g = QC.testProperty name (eqProp (-1) f g)

safeEval :: forall a. a -> Maybe a
safeEval x = unsafePerformIO $ do
  r <- try (evaluate x) :: IO (Either SomeException a)
  pure (either (const Nothing) Just r)
