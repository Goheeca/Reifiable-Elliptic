{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}

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

reifiedGaloisPrimeField :: Integer -> Def Field.C Integer
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


reifiedEllipticGroup :: Eq a => Def Field.C a -> a -> Def AbelianGroup.C (a,a)
reifiedEllipticGroup field linearCoeff =
  -- AbelianGroup: (a, 0, +, -)
  ReifiedAbelianGroup
    -- the neutral element of addition
    (fieldZero, fieldZero)
    -- the add operation
    (\p@(x_p, y_p) q@(x_q, y_q) ->
       let infinityPoint = (fieldZero, fieldZero)
           verticalSlope | p == q    = y_p == fieldZero
                         | otherwise = inField (x_q - x_p) == fieldZero
           two = inField $ one + one
           three = inField $ one + two
           slope | p == q    = inField $ (three * x_p ^ 2 + linearCoeff) / (two * y_p)
                 | otherwise = inField $ (y_q - y_p) / (x_q - x_p)
           x_r = inField $ slope ^ 2 - x_p - x_q
           y_r = inField $ slope * (x_p - x_r) - y_p
       in if p == infinityPoint
          then q
          else if q == infinityPoint
               then p
               else if verticalSlope
                    then infinityPoint
                    else (x_r, y_r))  
    -- the additive inverse operation
    (\(x, y) -> (x, inField $ negate y))
    where inField = using field
          fieldZero = inField zero
