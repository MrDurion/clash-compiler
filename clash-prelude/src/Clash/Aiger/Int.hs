module Clash.Aiger.Int where

import Prelude hiding (abs, maxBound, minBound, negate, signum, (*), (+), (-))

import Clash.Aiger.Base (as)
import Clash.Sized.Internal.Signed (Signed)

import qualified Clash.Aiger.Signed as S

-- Eq and Ord
eq
  , neq
  , lt
  , le
  , gt
  , ge ::
    Int -> Int -> Bool
eq (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.eq s1 s2
neq (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.neq s1 s2
lt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.lt s1 s2
le (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.le s1 s2
gt (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.gt s1 s2
ge (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = S.ge s1 s2

-- Num
(+), (-), (*) :: Int -> Int -> Int
(+) (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = as @Int $ s1 S.+ s2
(-) (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = as @Int $ s1 S.- s2
(*) (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = as @Int $ s1 S.* s2

negate, abs, signum :: Int -> Int
negate (as @(Signed 64) -> s) = as @Int $ S.negate s
abs (as @(Signed 64) -> s) = as @Int $ S.abs s
signum (as @(Signed 64) -> s) = as @Int $ S.signum s

-- Undefined
undefined# :: Int
undefined# = as @Int S.undefined#

-- Bounded
minBound, maxBound :: Int
minBound = as @Int $ S.minBound
maxBound = as @Int $ S.maxBound

-- Bits
and, or, xor :: Int -> Int -> Int
and (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = as @Int $ S.and s1 s2
or (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = as @Int $ S.or s1 s2
xor (as @(Signed 64) -> s1) (as @(Signed 64) -> s2) = as @Int $ S.xor s1 s2

complement :: Int -> Int
complement (as @(Signed 64) -> s) = as @Int $ S.complement s

zeroBits :: Int
zeroBits = as @Int $ S.zeroBits

bit :: Int -> Int
bit i = as @Int $ S.bit i

testBit :: Int -> Int -> Bool
testBit (as @(Signed 64) -> s) i = S.testBit s i

setBit, clearBit, complementBit :: Int -> Int -> Int
setBit (as @(Signed 64) -> s) i = as @Int $ S.setBit s i
clearBit (as @(Signed 64) -> s) i = as @Int $ S.clearBit s i
complementBit (as @(Signed 64) -> s) i = as @Int $ S.complementBit s i

isSigned :: Int -> Bool
isSigned (as @(Signed 64) -> s) = S.isSigned s

bitSizeMaybe :: Int -> Maybe Int
bitSizeMaybe (as @(Signed 64) -> s) = S.bitSizeMaybe s

bitSize :: Int -> Int
bitSize (as @(Signed 64) -> s) = S.bitSize s

popCount :: Int -> Int
popCount (as @(Signed 64) -> s) = S.popCount s

shiftL, shiftR, rotateL, rotateR :: Int -> Int -> Int
shiftL (as @(Signed 64) -> s) i = as @Int $ S.shiftL s i
shiftR (as @(Signed 64) -> s) i = as @Int $ S.shiftL s i
rotateL (as @(Signed 64) -> s) i = as @Int $ S.rotateL s i
rotateR (as @(Signed 64) -> s) i = as @Int $ S.rotateR s i
