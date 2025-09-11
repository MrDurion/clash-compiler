module Clash.Aiger.Int where

import Prelude hiding (abs, maxBound, minBound, negate, signum, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Signed as S

-- Util
asSigned :: Int -> Signed 64
asSigned i = as @(Signed 64) i

fromSigned :: Signed 64 -> Int
fromSigned s = as @(Int) s

-- Int

eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    Int -> Int -> Bool
(+), (-), (*) :: Int -> Int -> Int
negate, abs, signum :: Int -> Int
undefined# :: Int
minBound, maxBound :: Int
and, or, xor :: Int -> Int -> Int
complement :: Int -> Int
zeroBits :: Int
bit :: Int -> Int
testBit :: Int -> Int -> Bool
bitSizeMaybe :: Int -> Maybe Int
bitSize, popCount :: Int -> Int
isSigned :: Int -> Bool
setBit
  , clearBit
  , complementBit
  , shiftL
  , shiftR
  , rotateL
  , rotateR ::
    Int -> Int -> Int
-- Eq and Ord
eq (asSigned -> s1) (asSigned -> s2) = S.eq s1 s2
neq (asSigned -> s1) (asSigned -> s2) = S.neq s1 s2
lt (asSigned -> s1) (asSigned -> s2) = S.lt s1 s2
le (asSigned -> s1) (asSigned -> s2) = S.le s1 s2
gt (asSigned -> s1) (asSigned -> s2) = S.gt s1 s2
ge (asSigned -> s1) (asSigned -> s2) = S.ge s1 s2

-- Num
(+) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.+ s2
(-) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.- s2
(*) (asSigned -> s1) (asSigned -> s2) = fromSigned $ s1 S.* s2
negate (asSigned -> s) = fromSigned $ S.negate s
abs (asSigned -> s) = fromSigned $ S.abs s
signum (asSigned -> s) = fromSigned $ S.signum s

-- Undefined
undefined# = fromSigned S.undefined#

-- Bounded
minBound = fromSigned $ S.minBound
maxBound = fromSigned $ S.maxBound

-- Bits
and (asSigned -> s1) (asSigned -> s2) = fromSigned $ S.and s1 s2
or (asSigned -> s1) (asSigned -> s2) = fromSigned $ S.or s1 s2
xor (asSigned -> s1) (asSigned -> s2) = fromSigned $ S.xor s1 s2
complement (asSigned -> s) = fromSigned $ S.complement s
zeroBits = fromSigned $ S.zeroBits
bit i = fromSigned $ S.bit i
testBit (asSigned -> s) i = S.testBit s i
setBit (asSigned -> s) i = fromSigned $ S.setBit s i
clearBit (asSigned -> s) i = fromSigned $ S.clearBit s i
complementBit (asSigned -> s) i = fromSigned $ S.complementBit s i
isSigned (asSigned -> s) = S.isSigned s
bitSizeMaybe (asSigned -> s) = S.bitSizeMaybe s
bitSize (asSigned -> s) = S.bitSize s
popCount (asSigned -> s) = S.popCount s
shiftL (asSigned -> s) i = fromSigned $ S.shiftL s i
shiftR (asSigned -> s) i = fromSigned $ S.shiftL s i
rotateL (asSigned -> s) i = fromSigned $ S.rotateL s i
rotateR (asSigned -> s) i = fromSigned $ S.rotateR s i
