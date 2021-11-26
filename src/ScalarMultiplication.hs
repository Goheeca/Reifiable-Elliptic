{-# LANGUAGE NoImplicitPrelude #-}

-- | A fast scalar multiplication of group elements.
module ScalarMultiplication (
  (+*), (<*>*)
) where

import NumericPrelude.Numeric
import NumericPrelude.Base

import qualified Algebra.Additive as AbelianGroup
import qualified Algebra.Monoid as Monoid
import qualified Number.NonNegative as NN


-- | This is a scalar multiplication which allows us to write
-- @ g + g + g @ as @ g +* 3 @.
(+*) :: AbelianGroup.C a
        => a             -- ^ The group element being acted upon
        -> NN.Integer    -- ^ The count of operands
        -> a             -- ^ The result of summing
(+*) elem power
  | power == 0 = zero
  | power == 1 = elem
  | even power = (elem + elem) +* (power `quot` NN.fromNumber 2)
  | otherwise  = elem + (elem +* (power - NN.fromNumber 1))

(<*>*) :: Monoid.C a => a -> NN.Integer -> a
(<*>*) elem power
  | power == 0 = Monoid.idt
  | power == 1 = elem
  | even power = (elem Monoid.<*> elem) <*>* (power `quot` NN.fromNumber 2)
  | otherwise  = elem Monoid.<*> (elem <*>* (power - NN.fromNumber 1))
