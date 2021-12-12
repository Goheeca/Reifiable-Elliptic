{-# LANGUAGE NoImplicitPrelude, Rank2Types, IncoherentInstances #-}

-- | A fast scalar multiplication of group elements.
module ScalarMultiplication (
  (+*),
  (<*>*),
  (*^),
  montgomeryLadder,
) where

import NumericPrelude.Numeric
import NumericPrelude.Base

import qualified Algebra.Field as Field
import qualified Algebra.Additive as AbelianGroup
import qualified Algebra.Monoid as Monoid
import qualified Algebra.Ring as Ring
import qualified Number.NonNegative as NN
import Math.NumberTheory.Logarithms (integerLog2)
import Data.Bits (testBit)

import ReifiedElliptic (EllipticPoint, Montgomery)

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

(*^) :: Ring.C a
        => a
        -> NN.Integer
        -> a
(*^) elem power
  | power == 0 = zero
  | power == 1 = elem
  | even power = (elem * elem) *^ (power `quot` NN.fromNumber 2)
  | otherwise  = elem * (elem *^ (power - NN.fromNumber 1))


montgomeryLadder :: Show a => ((Field.C a => a) -> a) -> a -> EllipticPoint Montgomery a -> NN.Integer -> EllipticPoint Montgomery a
montgomeryLadder inField aCoeff projectedPoint power
  | power == 0 = (inField one, inField zero)
  | power == 1 = projectedPoint
  | otherwise = go projectedPoint (double aCoeff projectedPoint) (integerLog2 (toInteger power) - 1)
  where two' = inField $ one + one
        four' = inField $ two' + two'
        add (x_p, z_p) (x_q, z_q) (x_diff, z_diff) = (inField (z_diff * ((x_p - z_p) * (x_q + z_q) + (x_p + z_p) * (x_q - z_q))^2), inField (x_diff * ((x_p - z_p) * (x_q + z_q) - (x_p + z_p) * (x_q - z_q))^2))
        double a (x_p, z_p) = (inField ((x_p + z_p)^2 * (x_p - z_p)^2), inField ((four' * x_p * z_p) * ((x_p - z_p)^2 + ((a + two') / four') * (four' * x_p * z_p))))
        go (x_p, z_p) _ (-1) = (inField (x_p / z_p), inField one)
        go p q bitPosition = if testBit (toInteger power) bitPosition
                             then go (add p q projectedPoint) (double aCoeff q) (bitPosition - 1)
                             else go (double aCoeff p) (add p q projectedPoint) (bitPosition - 1)

