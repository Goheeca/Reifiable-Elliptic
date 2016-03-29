{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}

-- | Making of an underlying finite field for the elliptic curve and the group of the curve itself.
module ReifiedElliptic (
  reifiedGaloisPrimeField,
  reifiedEllipticGroup
) where

import ReifiedAlgebra

import Control.Applicative (liftA)
import Math.NumberTheory.Moduli (invertMod)

import qualified Algebra.Field as Field
import qualified Algebra.Additive as AbelianGroup

import NumericPrelude.Numeric
import NumericPrelude.Base

import Data.Maybe

-- | The underlying field maker.
reifiedGaloisPrimeField :: Integer             -- ^ A prime number, the count of elements in the field
                        -> Def Field.C Integer -- ^ A finite field
reifiedGaloisPrimeField prime =
  -- Field: (Integer mod prime, 0, +, -, *, 1, ^-1)
  ReifiedField
    0                    -- the neutral element of addition
    (totalize2 . (+))    -- the add operation
    (totalize . negate)  -- the additive inverse operation
    (totalize2 . (*))    -- the multiply operation
    1                    -- the neutral element of multiplication
    inverse              -- the multiplicative inverse operation
    where totalize = (`mod` prime)
          totalize2 = liftA totalize
          inverse = \n -> case invertMod n prime of Just r -> r

-- | The maker of a group of the elliptic curve.
reifiedEllipticGroup :: Eq a
                     => Def Field.C a                    -- ^ An underlying field
                     -> a                                -- ^ The linear coefficient in the equation of the particular elliptic curve
                     -> Def AbelianGroup.C (Maybe (a,a)) -- ^ An elliptic group
reifiedEllipticGroup field linearCoeff =
  -- AbelianGroup: (a * a + Zero, 0, +, -)
  ReifiedAbelianGroup
    -- the neutral element of addition
    Nothing
    -- the add operation
    (\p' q' -> case (p', q') of
       (Nothing, q) -> q
       (p, Nothing) -> p
       (Just p@(x_p, y_p), Just q@(x_q, y_q)) ->
         let verticalSlope | p == q    = y_p == fieldZero
                           | otherwise = inField (x_q - x_p) == fieldZero
             two = inField $ one + one
             three = inField $ one + two
             slope | p == q    = inField $ (three * x_p ^ 2 + linearCoeff) / (two * y_p)
                   | otherwise = inField $ (y_q - y_p) / (x_q - x_p)
             x_r = inField $ slope ^ 2 - x_p - x_q
             y_r = inField $ slope * (x_p - x_r) - y_p
         in if verticalSlope
            then Nothing
            else Just (x_r, y_r))
    -- the additive inverse operation
    (\p -> case p of
       Nothing -> Nothing
       Just (x, y) -> Just (x, inField $ negate y))
    where inField = using field
          fieldZero = inField zero
