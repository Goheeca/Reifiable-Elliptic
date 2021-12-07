{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}

module IrreducibilityTest (
  isProbablyIrreducible,
) where

import NumericPrelude.Numeric
import NumericPrelude.Base
import Data.Bit (F2Poly)
import qualified Number.NonNegative as NN
import qualified Prelude (fromInteger)

import ReifiedElliptic
import Reificator (using)
import ScalarMultiplication ((*^))

isProbablyIrreducible :: Integer -> NN.Integer -> F2Poly -> Bool
isProbablyIrreducible maxNumberOfTests degree tested = all (notFermatWitness . Prelude.fromInteger) [2 .. min maxNumberOfTests ((2 ^ toInteger degree) `quot` 2)]
  where inField = using $ reifiedGaloisBinaryField tested
        notFermatWitness test = inField (test *^ (2 ^ toInteger degree)) == inField test