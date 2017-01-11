{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}

-- | Making of an underlying finite field for the elliptic curve and the group of the curve itself.
module ReifiedElliptic (
  reifiedGaloisPrimeField,
  reifiedGaloisBinaryField,
  reifiedStandardEllipticGroup,
  reifiedKoblitzEllipticGroup,
  EllipticPoint
) where

import ReifiedAlgebra

import Control.Applicative (liftA)
import Math.NumberTheory.Moduli (invertMod)

import qualified Algebra.Field as Field
import qualified Algebra.Additive as AbelianGroup
import qualified Algebra.Monoid as Monoid

import NumericPrelude.Numeric
import NumericPrelude.Base

import Data.Maybe

import Data.Bits
import BinaryPolynomialOperations

type EllipticPoint a = Maybe (a,a)

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

-- GF(2^n)
reifiedGaloisBinaryField :: Integer -> Integer -> Def Field.C Integer
reifiedGaloisBinaryField degree modulus =
  -- Field: (P(degree - 1) mod modulus, 0, +, -, *, 1, ^-1)
  ReifiedField
    0
    xor
    (xor maxVal)
    (polMult modulus)
    1
    (polInv modulus)
    where maxVal = 2 ^ degree - 1


-- | The maker of a group of the elliptic curve.
reifiedStandardEllipticGroup :: Eq a
                             => Def Field.C a                    -- ^ An underlying field
                             -> a                                -- ^ The linear coefficient in the equation of the particular elliptic curve
                             -> Def AbelianGroup.C (EllipticPoint a) -- ^ An elliptic group
reifiedStandardEllipticGroup field linearCoeff =
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

reifiedKoblitzEllipticGroup :: Eq a => Def Field.C a -> a -> Def AbelianGroup.C (EllipticPoint a)
reifiedKoblitzEllipticGroup field linearCoeff =
  -- AbelianGroup: (a * a + Zero, 0, +, -)
  ReifiedAbelianGroup
    -- the neutral element of addition
    Nothing
    -- the add operation
    (\p' q' -> case (p', q') of
       (Nothing, q) -> q
       (p, Nothing) -> p
       (Just (x_p, y_p), Just (x_q, y_q)) ->
         if x_p == x_q
         then if inField (y_p + y_q) == x_q
              then Nothing
              else let slope = inField $ x_q + y_q / x_q
                       x_r = inField $ slope ^ 2 + slope + linearCoeff
                       y_r = inField $ x_q ^ 2 + (slope + one) * x_r
                   in Just (x_r, y_r)
         else let slope = inField $ (y_p + y_q) / (x_p + x_q)
                  x_r = inField $ slope ^ 2 + slope + x_p + x_q + linearCoeff
                  y_r = inField $ (x_q + x_r) * slope + x_r + y_q
              in Just (x_r, y_r))
    -- the additive inverse operation
    (\p -> case p of
       Nothing -> Nothing
       Just (x, y) -> Just (x, inField $ x + y))
    where inField = using field
          fieldZero = inField zero
