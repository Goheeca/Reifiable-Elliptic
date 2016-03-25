{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}

module ReifiedElliptic (
  reifiedGaloisPrimeField,
  reifiedGaloisBinaryField,
  reifiedEllipticGroup
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
import Data.List (replicate)
import ScalarMultiplication

-- GF(p)
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

-- GF(2^n)
reifiedGaloisBinaryField :: Integer -> Integer -> Def Field.C Integer
reifiedGaloisBinaryField degree modulus =
  -- Field: (P(degree - 1) mod modulus, 0, +, -, *, 1, ^-1)
  ReifiedField
    0
    xor
    (xor maxVal)
    mult
    1
    inv
    where maxVal = 2 ^ degree - 1
          mult = mult_ degree 0
          mult_ 0 p _ _ = p
          mult_ n p a b =
            let shiftA = shift a 1
            in mult_ (n - 1)
                     (if testBit b 0 then p `xor` a else p)
                     (if testBit shiftA (fromInteger degree) then shiftA `xor` modulus else shiftA)
                     (clearBit (shift b (-1)) (fromInteger degree))
          inv a = using (ReifiedMonoid 1 mult) $ a <*>* (fromInteger (maxVal - 1))


reifiedEllipticGroup :: Eq a => Def Field.C a -> a -> Def AbelianGroup.C (Maybe (a,a))
reifiedEllipticGroup field linearCoeff =
  -- AbelianGroup: (a, 0, +, -)
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
