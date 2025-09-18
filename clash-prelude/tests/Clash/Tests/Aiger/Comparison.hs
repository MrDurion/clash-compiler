{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}

module Clash.Tests.Aiger.Comparison (tests, main) where

import Data.Bits (Bits (..))
import Data.Data (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal, type (+))
import Test.Tasty

import Clash.Aiger.BitVector (lsb, msb, reduceAnd, reduceOr, reduceXor)
import Clash.Class.Resize (Resize (..))
import Clash.Sized.Internal.BitVector (Bit, BitVector)
import Clash.Sized.Internal.Signed (Signed)
import Clash.Sized.Internal.Unsigned (Unsigned)
import Clash.Tests.Aiger.EqualTests

import qualified Clash.Aiger.Bit as AIGER_BIT
import qualified Clash.Aiger.BitVector as AIGER
import qualified Clash.Aiger.Signed as AIGER_S
import qualified Clash.Aiger.Unsigned as AIGER_U

tests :: TestTree
tests =
  testGroup "Aiger comparison tests" $
    [ testGroup "Bit" $ bitTests
    , testGroup "BitVector 0" $ bitvectorTestsWithNas @0
    , testGroup "BitVector 1" $ bitvectorTestsWithNas @1
    , testGroup "BitVector 2" $ bitvectorTestsWithNas @2
    , testGroup "BitVector 4" $ bitvectorTestsWithNas @4
    , testGroup "BitVector 16" $ bitvectorTestsWithNas @16
    , testGroup "Unsigned 0" $ unsignedTestsWithNas @0
    , testGroup "Unsigned 1" $ unsignedTestsWithNas @1
    , testGroup "Unsigned 2" $ unsignedTestsWithNas @2
    , testGroup "Unsigned 4" $ unsignedTestsWithNas @4
    , testGroup "Unsigned 16" $ unsignedTestsWithNas @16
    , testGroup "Signed 0" $ signedTestsWithNas @0
    , testGroup "Signed 1" $ signedTestsWithNas @1
    , testGroup "Signed 2" $ signedTestsWithNas @2
    , testGroup "Signed 4" $ signedTestsWithNas @4
    , testGroup "Signed 16" $ signedTestsWithNas @16
    ]

bitTests :: [TestTree]
bitTests =
  [ -- Eq
    eqTest "Bit neq" (AIGER_BIT.neq) ((/=) @(Bit))
  , eqTest "Bit eq" AIGER_BIT.eq ((==) @(Bit))
  , -- Ord
    eqTest "Bit lt" AIGER_BIT.lt ((<) @(Bit))
  , eqTest "Bit le" AIGER_BIT.le ((<=) @(Bit))
  , eqTest "Bit gt" AIGER_BIT.gt ((>) @(Bit))
  , eqTest "Bit ge" AIGER_BIT.ge ((>=) @(Bit))
  , -- Bit
    eqTest "Bit and" AIGER_BIT.and ((.&.) @(Bit))
  , eqTest "Bit or" AIGER_BIT.or ((.|.) @(Bit))
  , eqTest "Bit xor" AIGER_BIT.xor (xor @(Bit))
  , eqTest "Bit complement" AIGER_BIT.complement (complement @(Bit))
  , eqTest "Bit bitSize" AIGER_BIT.bitSize (bitSize @(Bit))
  , eqTest "Bit bitSizeMaybe" AIGER_BIT.bitSizeMaybe (bitSizeMaybe @(Bit))
  , eqTest "Bit isSigned" AIGER_BIT.isSigned (isSigned @(Bit))
  , eqTest "Bit zeroBits" AIGER_BIT.zeroBits (zeroBits @(Bit))
  , eqTestBoundedInt @1 "Bit bit" AIGER_BIT.bit (bit @(Bit))
  , eqTestBoundedInt @1 "Bit setBit" AIGER_BIT.setBit (setBit @(Bit))
  , eqTestBoundedInt @1 "Bit clearBit" AIGER_BIT.clearBit (clearBit @(Bit))
  , eqTestBoundedInt @1
      "Bit complementBit"
      AIGER_BIT.complementBit
      (complementBit @(Bit))
  , eqTestBoundedInt @1 "Bit testBit" AIGER_BIT.testBit (testBit @(Bit))
  , eqTest "Bit shift" AIGER_BIT.shift (shift @(Bit))
  , eqTest "Bit shiftL" AIGER_BIT.shiftL (shiftL @(Bit))
  , eqTest "Bit shiftR" AIGER_BIT.shiftR (shiftR @(Bit))
  , eqTest "Bit rotate" AIGER_BIT.rotate (rotate @(Bit))
  , eqTest "Bit rotateL" AIGER_BIT.rotateL (rotateL @(Bit))
  , eqTest "Bit rotateR" AIGER_BIT.rotateR (rotateR @(Bit))
  , eqTest "Bit popCount" AIGER_BIT.popCount (popCount @(Bit))
  , -- Num
    eqTest "Bit +" (AIGER_BIT.+) ((+) @(Bit))
  , eqTest "Bit -" (AIGER_BIT.-) ((-) @(Bit))
  , eqTest "Bit *" (AIGER_BIT.*) ((*) @(Bit))
  , eqTest "Bit negate" AIGER_BIT.negate (negate @(Bit))
  , eqTest "Bit abs" AIGER_BIT.abs (abs @(Bit))
  , eqTest "Bit signum" AIGER_BIT.signum (signum @(Bit))
  , -- Bounded
    eqTest "Bit minBound" AIGER_BIT.minBound (minBound @(Bit))
  , eqTest "Bit maxBound" AIGER_BIT.maxBound (maxBound @(Bit))
  ]

bitvectorTestsWithNas :: forall n. (KnownNat n) => [TestTree]
bitvectorTestsWithNas =
  [ -- Eq
    eqTest
      (show (natVal (Proxy @n)) ++ " BitVector neq")
      (AIGER.neq)
      ((/=) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector eq")
      (AIGER.eq)
      ((==) @(BitVector n))
  , -- Ord
    eqTest
      (show (natVal (Proxy @n)) ++ " BitVector lt")
      AIGER.lt
      ((<) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector le")
      AIGER.le
      ((<=) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector gt")
      AIGER.gt
      ((>) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector ge")
      AIGER.ge
      ((>=) @(BitVector n))
  , -- Bit
    eqTest
      (show (natVal (Proxy @n)) ++ " BitVector and")
      AIGER.and
      ((.&.) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector or")
      AIGER.or
      ((.|.) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector xor")
      AIGER.xor
      (xor @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector complement")
      AIGER.complement
      (complement @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector bitSize")
      AIGER.bitSize
      (bitSize @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector bitSizeMaybe")
      AIGER.bitSizeMaybe
      (bitSizeMaybe @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector isSigned")
      AIGER.isSigned
      (isSigned @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector zeroBits")
      AIGER.zeroBits
      (zeroBits @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector shiftL")
      AIGER.shiftL
      (shiftL @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector shiftR")
      AIGER.shiftR
      (shiftR @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector popCount")
      AIGER.popCount
      (popCount @(BitVector n))
  , -- Num
    eqTest
      (show (natVal (Proxy @n)) ++ " BitVector +")
      (AIGER.+)
      ((+) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector -")
      (AIGER.-)
      ((-) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector *")
      (AIGER.*)
      ((*) @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector negate")
      AIGER.negate
      (negate @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector abs")
      AIGER.abs
      (abs @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector signum")
      AIGER.signum
      (signum @(BitVector n))
  , -- Bounded
    eqTest
      (show (natVal (Proxy @n)) ++ " BitVector minBound")
      AIGER.minBound
      (minBound @(BitVector n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector maxBound")
      AIGER.maxBound
      (maxBound @(BitVector n))
  , -- Resize
    eqTest
      (show (natVal (Proxy @n)) ++ " BitVector truncateB 1")
      (AIGER.truncateB @n @1)
      truncateB
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector truncateB 4")
      (AIGER.truncateB @n @4)
      truncateB
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector zeroExtend 1")
      (AIGER.zeroExtend @n @(n + 1))
      zeroExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector zeroExtend 4")
      (AIGER.zeroExtend @n @(n + 4))
      zeroExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector signExtend 1")
      (AIGER.signExtend @n @(n + 1))
      signExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector signExtend 4")
      (AIGER.signExtend @n @(n + 4))
      signExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector resize +1")
      (AIGER.resize @n @(n + 1))
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector resize +4")
      (AIGER.resize @n @(n + 4))
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector resize -1")
      (AIGER.resize @(n + 1) @n)
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector resize -4")
      (AIGER.resize @(n + 4) @n)
      resize
  , -- Other
    eqTest
      (show (natVal (Proxy @n)) ++ " BitVector reduceAnd")
      (AIGER.reduceAnd @n)
      (reduceAnd)
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector reduceOr")
      (AIGER.reduceOr @n)
      (reduceOr)
  , eqTest
      (show (natVal (Proxy @n)) ++ " BitVector reduceXor")
      (AIGER.reduceXor @n)
      (reduceXor)
  , eqTest (show (natVal (Proxy @n)) ++ " BitVector msb") (AIGER.msb @n) (msb)
  , eqTest (show (natVal (Proxy @n)) ++ " BitVector lsb") (AIGER.lsb @n) (lsb)
  ]
    ++ if natVal (Proxy @n) == 0
      then []
      else
        [ eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " BitVector bit")
            AIGER.bit
            (bit @(BitVector n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " BitVector setBit")
            AIGER.setBit
            (setBit @(BitVector n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " BitVector clearBit")
            AIGER.clearBit
            (clearBit @(BitVector n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " BitVector complementBit")
            AIGER.complementBit
            (complementBit @(BitVector n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " BitVector testBit")
            AIGER.testBit
            (testBit @(BitVector n))
        , eqTest
            (show (natVal (Proxy @n)) ++ " BitVector rotateL")
            AIGER.rotateL
            (rotateL @(BitVector n))
        , eqTest
            (show (natVal (Proxy @n)) ++ " BitVector rotateR")
            AIGER.rotateR
            (rotateR @(BitVector n))
        ]

unsignedTestsWithNas :: forall n. (KnownNat n) => [TestTree]
unsignedTestsWithNas =
  [ -- Eq
    eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned neq")
      (AIGER_U.neq)
      ((/=) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned eq")
      (AIGER_U.eq)
      ((==) @(Unsigned n))
  , -- Ord
    eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned lt")
      AIGER_U.lt
      ((<) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned le")
      AIGER_U.le
      ((<=) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned gt")
      AIGER_U.gt
      ((>) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned ge")
      AIGER_U.ge
      ((>=) @(Unsigned n))
  , -- Bit
    eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned and")
      AIGER_U.and
      ((.&.) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned or")
      AIGER_U.or
      ((.|.) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned xor")
      AIGER_U.xor
      (xor @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned complement")
      AIGER_U.complement
      (complement @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned bitSize")
      AIGER_U.bitSize
      (bitSize @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned bitSizeMaybe")
      AIGER_U.bitSizeMaybe
      (bitSizeMaybe @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned isSigned")
      AIGER_U.isSigned
      (isSigned @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned zeroBits")
      AIGER_U.zeroBits
      (zeroBits @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned shiftL")
      AIGER_U.shiftL
      (shiftL @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned shiftR")
      AIGER_U.shiftR
      (shiftR @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned popCount")
      AIGER_U.popCount
      (popCount @(Unsigned n))
  , -- Num
    eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned +")
      (AIGER_U.+)
      ((+) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned -")
      (AIGER_U.-)
      ((-) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned *")
      (AIGER_U.*)
      ((*) @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned negate")
      AIGER_U.negate
      (negate @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned abs")
      AIGER_U.abs
      (abs @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned signum")
      AIGER_U.signum
      (signum @(Unsigned n))
  , -- Bounded
    eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned minBound")
      AIGER_U.minBound
      (minBound @(Unsigned n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned maxBound")
      AIGER_U.maxBound
      (maxBound @(Unsigned n))
  , -- Resize
    eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned truncateB 1")
      (AIGER_U.truncateB @n @1)
      truncateB
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned truncateB 4")
      (AIGER_U.truncateB @n @4)
      truncateB
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned zeroExtend 1")
      (AIGER_U.zeroExtend @n @(n + 1))
      zeroExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned zeroExtend 4")
      (AIGER_U.zeroExtend @n @(n + 4))
      zeroExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned signExtend 1")
      (AIGER_U.signExtend @n @(n + 1))
      signExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned signExtend 4")
      (AIGER_U.signExtend @n @(n + 4))
      signExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned resize +1")
      (AIGER_U.resize @n @(n + 1))
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned resize +4")
      (AIGER_U.resize @n @(n + 4))
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned resize -1")
      (AIGER_U.resize @(n + 1) @n)
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " Unsigned resize -4")
      (AIGER_U.resize @(n + 4) @n)
      resize
  ]
    ++ if natVal (Proxy @n) == 0
      then []
      else
        [ eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Unsigned bit")
            AIGER_U.bit
            (bit @(Unsigned n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Unsigned setBit")
            AIGER_U.setBit
            (setBit @(Unsigned n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Unsigned clearBit")
            AIGER_U.clearBit
            (clearBit @(Unsigned n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Unsigned complementBit")
            AIGER_U.complementBit
            (complementBit @(Unsigned n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Unsigned testBit")
            AIGER_U.testBit
            (testBit @(Unsigned n))
        , eqTest
            (show (natVal (Proxy @n)) ++ " Unsigned rotateL")
            AIGER_U.rotateL
            (rotateL @(Unsigned n))
        , eqTest
            (show (natVal (Proxy @n)) ++ " Unsigned rotateR")
            AIGER_U.rotateR
            (rotateR @(Unsigned n))
        ]

signedTestsWithNas :: forall n. (KnownNat n) => [TestTree]
signedTestsWithNas =
  [ -- Eq
    eqTest
      (show (natVal (Proxy @n)) ++ " Signed neq")
      (AIGER_S.neq)
      ((/=) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed eq")
      (AIGER_S.eq)
      ((==) @(Signed n))
  , -- Ord
    eqTest
      (show (natVal (Proxy @n)) ++ " Signed lt")
      AIGER_S.lt
      ((<) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed le")
      AIGER_S.le
      ((<=) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed gt")
      AIGER_S.gt
      ((>) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed ge")
      AIGER_S.ge
      ((>=) @(Signed n))
  , -- Bit
    eqTest
      (show (natVal (Proxy @n)) ++ " Signed and")
      AIGER_S.and
      ((.&.) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed or")
      AIGER_S.or
      ((.|.) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed xor")
      AIGER_S.xor
      (xor @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed complement")
      AIGER_S.complement
      (complement @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed bitSize")
      AIGER_S.bitSize
      (bitSize @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed bitSizeMaybe")
      AIGER_S.bitSizeMaybe
      (bitSizeMaybe @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed isSigned")
      AIGER_S.isSigned
      (isSigned @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed zeroBits")
      AIGER_S.zeroBits
      (zeroBits @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed shiftL")
      AIGER_S.shiftL
      (shiftL @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed shiftR")
      AIGER_S.shiftR
      (shiftR @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed popCount")
      AIGER_S.popCount
      (popCount @(Signed n))
  , -- Num
    eqTest
      (show (natVal (Proxy @n)) ++ " Signed +")
      (AIGER_S.+)
      ((+) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed -")
      (AIGER_S.-)
      ((-) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed *")
      (AIGER_S.*)
      ((*) @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed negate")
      AIGER_S.negate
      (negate @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed abs")
      AIGER_S.abs
      (abs @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed signum")
      AIGER_S.signum
      (signum @(Signed n))
  , -- Bounded
    eqTest
      (show (natVal (Proxy @n)) ++ " Signed minBound")
      AIGER_S.minBound
      (minBound @(Signed n))
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed maxBound")
      AIGER_S.maxBound
      (maxBound @(Signed n))
  , -- Resize
    eqTest
      (show (natVal (Proxy @n)) ++ " Signed truncateB 1")
      (AIGER_S.truncateB @n @1)
      truncateB
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed truncateB 4")
      (AIGER_S.truncateB @n @4)
      truncateB
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed zeroExtend 1")
      (AIGER_S.zeroExtend @n @(n + 1))
      zeroExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed zeroExtend 4")
      (AIGER_S.zeroExtend @n @(n + 4))
      zeroExtend
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed resize +1")
      (AIGER_S.resize @n @(n + 1))
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed resize +4")
      (AIGER_S.resize @n @(n + 4))
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed resize -1")
      (AIGER_S.resize @(n + 1) @n)
      resize
  , eqTest
      (show (natVal (Proxy @n)) ++ " Signed resize -4")
      (AIGER_S.resize @(n + 4) @n)
      resize
  ]
    ++ if natVal (Proxy @n) == 0
      then []
      else
        [ eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Signed bit")
            AIGER_S.bit
            (bit @(Signed n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Signed setBit")
            AIGER_S.setBit
            (setBit @(Signed n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Signed clearBit")
            AIGER_S.clearBit
            (clearBit @(Signed n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Signed complementBit")
            AIGER_S.complementBit
            (complementBit @(Signed n))
        , eqTestBoundedInt @n
            (show (natVal (Proxy @n)) ++ " Signed testBit")
            AIGER_S.testBit
            (testBit @(Signed n))
        , eqTest
            (show (natVal (Proxy @n)) ++ " Signed rotateL")
            AIGER_S.rotateL
            (rotateL @(Signed n))
        , eqTest
            (show (natVal (Proxy @n)) ++ " Signed rotateR")
            AIGER_S.rotateR
            (rotateR @(Signed n))
        ]

main :: IO ()
main = defaultMain tests
