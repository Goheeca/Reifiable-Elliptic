{-# LANGUAGE MultiParamTypeClasses, Rank2Types, FlexibleContexts #-}

module RefinedElliptic (
  RefinedStandardCurveDef(..),
  RefinedKoblitzCurveDef(..),
  RefinedMontgomeryCurveDef(..),
  F2Poly,
) where

import ReifiedElliptic
import Elliptic
import UnrefinedElliptic
import Reificator (using)
import ScalarMultiplication

import Refined
import Math.NumberTheory.Primes.Testing (isCertifiedPrime)
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Logarithms (integerLog2)
import Data.Typeable (typeRep)
import Data.Text (pack)
import Data.Bit (F2Poly)
import EquationTest (standardEquationTest, koblitzEquationTest, montgomeryEquationTest)
import IrreducibilityTest (isProbablyIrreducible)


hasseTheorem :: Integer -> Integer -> Integer -> Bool
hasseTheorem cofactor order q = abs (cofactor * order - (q + 1)) <= 2 * (1 + integerSquareRoot q)

toIntgr :: Integral a => (b a -> a) -> b a -> Integer
toIntgr fval def = fromIntegral $ fval def

refiner_s :: Integral a => (UnrefinedStandardCurveDef a -> a) -> RefinedStandardCurveDef a -> Integer
refiner_s fval = toIntgr $ fval . unrefine . unRefinedStandardCurveDef

refiner_m :: Integral a => (UnrefinedMontgomeryCurveDef a -> a) -> RefinedMontgomeryCurveDef a -> Integer
refiner_m fval = toIntgr $ fval . unrefine . unRefinedMontgomeryCurveDef

toBinPoly :: (Integral a, Integral b) => (c a -> b) -> c a -> F2Poly
toBinPoly fval def = fromInteger . toInteger $ fval def

refiner_k :: (Integral a, Integral b) => (UnrefinedKoblitzCurveDef a -> b) -> RefinedKoblitzCurveDef a -> F2Poly
refiner_k fval = toBinPoly $ fval . unrefine . unRefinedKoblitzCurveDef

data StandardCurveDefValidator

instance Integral a => Predicate StandardCurveDefValidator (UnrefinedStandardCurveDef a) where
  validate pred def
    | not $ isCertifiedPrime . toInteger $ prime_s def = Just $ RefineOtherException (typeRep pred) (pack "Modulus isn't prime")
    | 0 >= toInteger (cofactor_s def) = Just $ RefineOtherException (typeRep pred) (pack "Cofactor isn't strictly positive")
    | -16 * (4 * aCoeff_s def ^ 3 + 27 * bCoeff_s def ^ 2) == 0 = Just $ RefineOtherException (typeRep pred) (pack "The curve is singular")
    | not $ hasseTheorem (toInteger $ cofactor_s def) (toInteger $ order_s def) (toInteger $ prime_s def) = Just $ RefineOtherException (typeRep pred) (pack "Hasse's theorem not satisfied")
    -- TODO check number of points exactly via Schoof algorithm or SEA algorithm
    | not $ standardEquationTest (toIntgr prime_s def) (toIntgr aCoeff_s def) (toIntgr bCoeff_s def) x y = Just $ RefineOtherException (typeRep pred) (pack "The generator doesn't lie on the curve")
    | otherwise = Nothing
    where x = fromIntegral $ fst . generator_s $ def
          y = fromIntegral $ snd . generator_s $ def

type RefinedStandardCurveDef' a = Refined StandardCurveDefValidator (UnrefinedStandardCurveDef a)
-- | A definition of the elliptic curve
newtype RefinedStandardCurveDef a = RefinedStandardCurveDef { unRefinedStandardCurveDef :: (RefinedStandardCurveDef' a)}


data KoblitzCurveDefValidator

instance Integral a => Predicate KoblitzCurveDefValidator (UnrefinedKoblitzCurveDef a) where
  validate pred def
    | integerLog2 (toInteger $ modulus_k def) /= fromIntegral (degree_k def) = Just $ RefineOtherException (typeRep pred) (pack "The degree doesn't agree with the reduction polynomial")
    | not $ isProbablyIrreducible 256 (degree_k def) (toBinPoly modulus_k def) = Just $ RefineOtherException (typeRep pred) (pack "The reduction polynomial isn't irreducible")
    | 0 >= toInteger (cofactor_k def) = Just $ RefineOtherException (typeRep pred) (pack "Cofactor isn't strictly positive")
    | - (bCoeff_k def + 12 * aCoeff_k def * bCoeff_k def + 48 * aCoeff_k def ^ 2 * bCoeff_k def + 64 * aCoeff_k def ^ 3 * bCoeff_k def + 432 * bCoeff_k def ^ 2) == 0 = Just $ RefineOtherException (typeRep pred) (pack "The curve is singular")
    | not $ hasseTheorem (toInteger $ cofactor_k def) (toInteger $ order_k def) (2 ^ toInteger (degree_k def)) = Just $ RefineOtherException (typeRep pred) (pack "Hasse's theorem not satisfied")
    -- TODO check number of points exactly via Schoof algorithm or SEA algorithm
    | not $ koblitzEquationTest (toBinPoly modulus_k def) (toBinPoly aCoeff_k def) (toBinPoly bCoeff_k def) x y = Just $ RefineOtherException (typeRep pred) (pack "The generator doesn't lie on the curve")
    | otherwise = Nothing
    where x = fromIntegral $ fst . generator_k $ def
          y = fromIntegral $ snd . generator_k $ def

type RefinedKoblitzCurveDef' a = Refined KoblitzCurveDefValidator (UnrefinedKoblitzCurveDef a)
-- | A definition of the elliptic curve
newtype RefinedKoblitzCurveDef a = RefinedKoblitzCurveDef { unRefinedKoblitzCurveDef :: (RefinedKoblitzCurveDef' a)}


data MontgomeryCurveDefValidator

instance Integral a => Predicate MontgomeryCurveDefValidator (UnrefinedMontgomeryCurveDef a) where
  validate pred def
    | not $ isCertifiedPrime . toInteger $ prime_m def = Just $ RefineOtherException (typeRep pred) (pack "Modulus isn't prime")
    | 0 >= toInteger (cofactor_m def) = Just $ RefineOtherException (typeRep pred) (pack "Cofactor isn't strictly positive")
    | toIntgr bCoeff_m def == 0 || toIntgr aCoeff_m def ^ 2 == 4 = Just $ RefineOtherException (typeRep pred) (pack "The curve is singular")
    | not $ hasseTheorem (toInteger $ cofactor_m def) (toInteger $ order_m def) (toInteger $ prime_m def) = Just $ RefineOtherException (typeRep pred) (pack "Hasse's theorem not satisfied")
    -- TODO check number of points exactly via Schoof algorithm or SEA algorithm
    | not $ montgomeryEquationTest (toIntgr prime_m def) (toIntgr aCoeff_m def) (toIntgr bCoeff_m def) u v = Just $ RefineOtherException (typeRep pred) (pack "The generator doesn't lie on the curve")
    | otherwise = Nothing
    where u = fromIntegral $ fst . generator_m $ def
          v = fromIntegral $ snd . generator_m $ def

type RefinedMontgomeryCurveDef' a = Refined MontgomeryCurveDefValidator (UnrefinedMontgomeryCurveDef a)
-- | A definition of the elliptic curve
newtype RefinedMontgomeryCurveDef a = RefinedMontgomeryCurveDef { unRefinedMontgomeryCurveDef :: (RefinedMontgomeryCurveDef' a)}


instance EllipticCurve RefinedStandardCurveDef Weierstrass Integer where
  generator = \def -> Just (refiner_s x_s def, refiner_s y_s def)
    where x_s = fst . generator_s
          y_s = snd . generator_s
  order = order_s . unrefine . unRefinedStandardCurveDef
  cofactor = cofactor_s . unrefine . unRefinedStandardCurveDef
  scalarMultiplication curve ellipticPoint scalar = inElliptic $ ellipticPoint +* scalar
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ refiner_s prime_s curve)
                            $ refiner_s aCoeff_s curve
          inElliptic = using ellipticGroup

instance EllipticCurve RefinedKoblitzCurveDef Weierstrass F2Poly where
  generator = \def -> Just (refiner_k x_k def, refiner_k y_k def)
    where x_k = fst . generator_k
          y_k = snd . generator_k
  order = order_k . unrefine . unRefinedKoblitzCurveDef
  cofactor = cofactor_k . unrefine . unRefinedKoblitzCurveDef
  scalarMultiplication curve ellipticPoint scalar = inElliptic $ ellipticPoint +* scalar
    where ellipticGroup = reifiedKoblitzEllipticGroup
                            (reifiedGaloisBinaryField $ refiner_k modulus_k curve)
                            $ refiner_k aCoeff_k curve
          inElliptic = using ellipticGroup

instance EllipticCurve RefinedMontgomeryCurveDef Montgomery Integer where
  generator = \def -> (refiner_m x_k def, 1)
    where x_k = fst . generator_m
  order = order_m . unrefine . unRefinedMontgomeryCurveDef
  cofactor = cofactor_m . unrefine . unRefinedMontgomeryCurveDef
  scalarMultiplication curve ellipticPoint scalar = montgomeryLadder inField (refiner_m aCoeff_m curve) ellipticPoint scalar
    where underlyingField = reifiedGaloisPrimeField $ refiner_m prime_m curve
          inField = using underlyingField


instance (Show a, Integral a) => Show (RefinedStandardCurveDef a) where
  show eC =
    "Galois Prime Field (p): " ++ show (refiner_s prime_s eC)
    ++ "\nCurve (a, b): y^2 = x^3 + " ++ show (refiner_s aCoeff_s eC) ++ "*x + " ++ show (refiner_s bCoeff_s eC)
    ++ "\nGenerator (G): " ++ show (generator eC)
    ++ "\nOrder (n): " ++ show (order eC)
    ++ "\nCofactor (h): " ++ show (cofactor eC)

instance (Show a, Integral a) => Show (RefinedKoblitzCurveDef a) where
  show eC =
    "Galois Binary Field (modulus): " ++ show (refiner_k modulus_k eC)
    ++ "\nCurve (a, b): y^2 + y*x = x^3 + " ++ show (refiner_k aCoeff_k eC) ++ "*x^2 + " ++ show (refiner_k bCoeff_k eC)
    ++ "\nGenerator (G): " ++ show (generator eC)
    ++ "\nOrder (n): " ++ show (order eC)
    ++ "\nCofactor (h): " ++ show (cofactor eC)

instance (Show a, Integral a) => Show (RefinedMontgomeryCurveDef a) where
  show eC =
    "Galois Prime Field (p): " ++ show (refiner_m prime_m eC)
    ++ "\nCurve (a, b): " ++ show (refiner_m bCoeff_m eC) ++ "*v^2 = u*(u^2 + " ++ show (refiner_m aCoeff_m eC) ++ "*u + 1)"
    ++ "\nProjectino of Generator (G): " ++ show (generator eC)
    ++ "\nOrder (n): " ++ show (order eC)
    ++ "\nCofactor (h): " ++ show (cofactor eC)