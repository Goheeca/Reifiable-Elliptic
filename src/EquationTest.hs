{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}

module EquationTest (
  standardEquationTest,
  koblitzEquationTest,
  montgomeryEquationTest,
) where

import NumericPrelude.Numeric
import NumericPrelude.Base
import Data.Bit (F2Poly)

import ReifiedElliptic
import Reificator (using)


standardEquationTest :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
standardEquationTest modulus a b x y = inField (y ^ 2 - (x ^ 3 + a * x + b)) == inField zero
  where inField = using $ reifiedGaloisPrimeField modulus

koblitzEquationTest :: F2Poly -> F2Poly -> F2Poly -> F2Poly -> F2Poly -> Bool
koblitzEquationTest modulus a b x y = inField (y ^ 2 + x * y - (x ^ 3 + a * x ^ 2 + b)) == inField zero
  where inField = using $ reifiedGaloisBinaryField modulus

montgomeryEquationTest :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
montgomeryEquationTest modulus a b u v = inField (b * v ^ 2 - u * (u ^ 2 + a * u + 1)) == inField zero
  where inField = using $ reifiedGaloisPrimeField modulus