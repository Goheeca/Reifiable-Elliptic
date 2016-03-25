{-# LANGUAGE NoImplicitPrelude #-}

module ScalarMultiplication (
  (+*), (<*>*)
) where

import NumericPrelude.Numeric
import NumericPrelude.Base

import qualified Algebra.Additive as AbelianGroup
import qualified Algebra.Monoid as Monoid
import qualified Number.NonNegative as NN

(+*) :: AbelianGroup.C a => a -> NN.Integer -> a
(+*) elem power
  | power == 0 = zero
  | power == 1 = elem
  | even power = (elem + elem) +* (power `quot` (NN.fromNumber 2))
  | otherwise  = elem + (elem +* (power - (NN.fromNumber 1)))

-- not generally safe
(<*>*) :: Monoid.C a => a -> NN.Integer -> a
(<*>*) elem power
  | power == 0 = Monoid.idt
  | power == 1 = elem
  | even power = (elem Monoid.<*> elem) <*>* (power `quot` (NN.fromNumber 2))
  | otherwise  = elem Monoid.<*> (elem <*>* (power - (NN.fromNumber 1)))
