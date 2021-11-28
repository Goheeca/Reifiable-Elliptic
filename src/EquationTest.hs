{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}

module EquationTest (
  standardEquationTest,
  koblitzEquationTest,
) where

import NumericPrelude.Numeric
import NumericPrelude.Base
import Data.Bit (F2Poly)

import ReifiedElliptic
import Reificator (using)

import Debug.Trace (trace)
import Numeric (showHex)
import qualified Prelude (toInteger)

debug = flip trace

standardEquationTest :: Integer -> Integer -> Integer -> Integer -> Integer -> Bool
standardEquationTest modulus a b x y = result `debug` ("m: " ++ show modulus ++ "\na: " ++ show a ++ "\nb: " ++ showHex b "" ++ "\nx: " ++ showHex x "" ++ "\ny: " ++ showHex y "" ++ "\nresult: " ++ show result ++ "\nleft: " ++ show left)
  where inField = using $ reifiedGaloisPrimeField modulus
        left = inField (y ^ 2 - (x ^ 3 + a * x + b))
        result = (left == inField zero)

koblitzEquationTest :: F2Poly -> F2Poly -> F2Poly -> F2Poly -> F2Poly -> Bool
koblitzEquationTest modulus a b x y = result `debug` ("m: " ++ show modulus ++ "\na: " ++ showHex (Prelude.toInteger a) "" ++ "\nb: " ++ showHex (Prelude.toInteger b) "" ++ "\nx: " ++ showHex (Prelude.toInteger x) "" ++ "\ny: " ++ showHex (Prelude.toInteger y) "" ++ "\nresult: " ++ show result ++ "\nleft: " ++ show left)
  where inField = using $ reifiedGaloisBinaryField modulus
        left = inField (y ^ 2 + x * y - (x ^ 3 + a * x ^ 2 + b))
        result = (left == inField zero)