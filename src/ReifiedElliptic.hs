{-# LANGUAGE NoImplicitPrelude, Rank2Types, FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Making of an underlying finite field for the elliptic curve and the group of the curve itself.
module ReifiedElliptic (
  reifiedGaloisPrimeField,
  reifiedGaloisBinaryField,
  reifiedStandardEllipticGroup,
  reifiedKoblitzEllipticGroup,
  EllipticPoint,
  Weierstrass,
  Montgomery,
  Edwards,
) where

import ReifiedAlgebra
import qualified Algebra.Field as Field
import qualified Algebra.Additive as AbelianGroup
import NumericPrelude.Numeric
import NumericPrelude.Base

import Math.NumberTheory.Moduli (invertSomeMod, modulo, getVal, SomeMod(..))
import GHC.Natural (naturalFromInteger)

import Data.Bit (F2Poly)
import qualified Prelude ((+))
import BinaryPolynomialOperations


data Weierstrass
data Montgomery
data Edwards
type family EllipticPoint form a
type instance EllipticPoint Weierstrass a = Maybe (a,a)
type instance EllipticPoint Montgomery Integer = Rational
type instance EllipticPoint Edwards a = (a, a)

-- | The underlying field maker. GF(p)
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
          totalize2 = fmap totalize
          inverse = \n -> case invertSomeMod (n `modulo` naturalFromInteger prime) of Just (SomeMod r) -> getVal r

-- | The underlying field maker. GF(2^d)
reifiedGaloisBinaryField :: F2Poly             -- ^ A modulus, a polynomial of degree d
                         -> Def Field.C F2Poly -- ^ A finite field
reifiedGaloisBinaryField modulus =
  -- Field: (P(d - 1) mod modulus, 0, +, -, *, 1, ^-1)
  ReifiedField
    0                 -- the neutral element of addition
    (Prelude.+)       -- the add operation
    id                -- the additive inverse operation
    (polMult modulus) -- the multiply operation
    1                 -- the neutral element of multiplication
    (polInv modulus)  -- the multiplicative inverse operation


-- | The maker of a group of the elliptic curve. <https://sefiks.com/2016/03/13/the-math-behind-elliptic-curve-cryptography/ The derivation>.
reifiedStandardEllipticGroup :: Eq a
                             => Def Field.C a                                    -- ^ An underlying field
                             -> a                                                -- ^ The linear coefficient in the equation of the particular elliptic curve
                             -> Def AbelianGroup.C (EllipticPoint Weierstrass a) -- ^ An elliptic group
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
         if verticalSlope
         then Nothing
         else let slope | p == q    = inField $ (three' * x_p ^ 2 + linearCoeff) / (two' * y_p)
                        | otherwise = inField $ (y_q - y_p) / (x_q - x_p)
                  x_r = inField $ slope ^ 2 - x_p - x_q
                  y_r = inField $ slope * (x_p - x_r) - y_p
              in Just (x_r, y_r)
         where zero' = inField zero
               two' = inField $ one + one
               three' = inField $ one + two'
               verticalSlope | p == q = y_p == zero'
                             | otherwise = inField (x_q - x_p) == zero')
    -- the additive inverse operation
    (\p -> case p of
       Nothing -> Nothing
       Just (x, y) -> Just (x, inField $ negate y))
    where inField = using field
          

-- | The maker of a group of the elliptic curve. <https://sefiks.com/2016/03/13/the-math-behind-elliptic-curves-over-binary-field/ The derivation>.
reifiedKoblitzEllipticGroup :: Eq a
                            => Def Field.C a                                    -- ^ An underlying field
                            -> a                                                -- ^ The quadratic coefficient in the equation of the particular elliptic curve
                            -> Def AbelianGroup.C (EllipticPoint Weierstrass a) -- ^ An elliptic group
reifiedKoblitzEllipticGroup field quadCoeff =
  -- AbelianGroup: (a * a + Zero, 0, +, -)
  ReifiedAbelianGroup
    -- the neutral element of addition
    Nothing
    -- the add operation
    (\p' q' -> case (p', q') of
       (Nothing, q) -> q
       (p, Nothing) -> p
       (Just p@(x_p, y_p), Just q@(x_q, y_q)) ->
         if verticalSlope
         then Nothing
         else let slope | p == q    = inField $ (three' * x_p ^ 2 + two' * quadCoeff * x_p - y_p) / (two' * y_p + x_p)
                        | otherwise = inField $ (y_q - y_p) / (x_q - x_p)
                  x_r = inField $ slope ^ 2 + slope - x_p - x_q - quadCoeff
                  y_r = inField $ slope * (x_p - x_r) - x_r - y_p
              in Just (x_r, y_r)
         where zero' = inField zero
               two' = inField $ one + one
               three' = inField $ one + two'
               verticalSlope | p == q = inField (two' * y_p - x_p) == zero'
                             | otherwise = inField (x_q - x_p) == zero')
    -- the additive inverse operation
    (\p -> case p of
       Nothing -> Nothing
       Just (x, y) -> Just (x, inField $ negate (x + y)))
    where inField = using field
