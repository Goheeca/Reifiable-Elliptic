{-# LANGUAGE MultiParamTypeClasses #-}

module RefinedElliptic (
  RefinedStandardCurveDef(..),
  RefinedKoblitzCurveDef(..),
  F2Poly,
) where

import ReifiedElliptic
import Elliptic
import UnrefinedElliptic
import Reificator (using)

import Refined
import Math.NumberTheory.Primes.Testing (isCertifiedPrime)
import Math.NumberTheory.Roots (integerSquareRoot)
import Math.NumberTheory.Logarithms (integerLog2)
import Data.Typeable (Typeable, typeOf)
import Data.Text (pack)
import Data.Bit (F2Poly)
import EquationTest (standardEquationTest, koblitzEquationTest)
import IrreducibilityTest (isProbablyIrreducible)


hasseTheorem :: Integer -> Integer -> Integer -> Bool
hasseTheorem cofactor order q = abs (cofactor * order - (q + 1)) <= 2 * (1 + integerSquareRoot q)

toIntgr :: Integral a => (b a -> a) -> b a -> Integer
toIntgr fval def = fromIntegral $ fval def

refiner_s :: Integral a => (UnrefinedStandardCurveDef a -> a) -> RefinedStandardCurveDef a -> Integer
refiner_s fval = toIntgr $ fval . unrefine . unRefinedStandardCurveDef

toBinPoly :: (Integral a, Integral b) => (c a -> b) -> c a -> F2Poly
toBinPoly fval def = fromInteger . toInteger $ fval def

refiner_k :: (Integral a, Integral b) => (UnrefinedKoblitzCurveDef a -> b) -> RefinedKoblitzCurveDef a -> F2Poly
refiner_k fval = toBinPoly $ fval . unrefine . unRefinedKoblitzCurveDef

data StandardCurveDefValidator

instance (Typeable a, Integral a) => Predicate StandardCurveDefValidator (UnrefinedStandardCurveDef a) where
  validate pred def
    | not $ isCertifiedPrime . toInteger $ prime_s def = Just $ RefineOtherException (typeOf pred) (pack "Modulus isn't prime")
    | 0 >= toInteger (cofactor_s def) = Just $ RefineOtherException (typeOf pred) (pack "Cofactor isn't strictly positive")
    | -16 * (4 * aCoeff_s def ^ 3 + 27 * bCoeff_s def ^ 2) == 0 = Just $ RefineOtherException (typeOf pred) (pack "The curve is singular")
    | not $ hasseTheorem (toInteger $ cofactor_s def) (toInteger $ order_s def) (toInteger $ prime_s def) = Just $ RefineOtherException (typeOf pred) (pack "Hasse's theorem not satisfied")
    -- TODO check number of points exactly via Schoof algorithm or SEA algorithm
    | not $ standardEquationTest (toIntgr prime_s def) (toIntgr aCoeff_s def) (toIntgr bCoeff_s def) x y = Just $ RefineOtherException (typeOf pred) (pack "The generator doesn't lie on the curve")
    | otherwise = Nothing
    where x = fromIntegral $ fst . generator_s $ def
          y = fromIntegral $ snd . generator_s $ def

type RefinedStandardCurveDef' a = Refined StandardCurveDefValidator (UnrefinedStandardCurveDef a)
-- | A definition of the elliptic curve
newtype RefinedStandardCurveDef a = RefinedStandardCurveDef { unRefinedStandardCurveDef :: (RefinedStandardCurveDef' a)}


data KoblitzCurveDefValidator

instance (Typeable a, Integral a) => Predicate KoblitzCurveDefValidator (UnrefinedKoblitzCurveDef a) where
  validate pred def
    | integerLog2 (toInteger $ modulus_k def) /= fromIntegral (degree_k def) = Just $ RefineOtherException (typeOf pred) (pack "The degree doesn't agree with the reduction polynomial")
    | not $ isProbablyIrreducible 256 (degree_k def) (toBinPoly modulus_k def) = Just $ RefineOtherException (typeOf pred) (pack "The reduction polynomial isn't irreducible")
    | 0 >= toInteger (cofactor_k def) = Just $ RefineOtherException (typeOf pred) (pack "Cofactor isn't strictly positive")
    | - (bCoeff_k def + 12 * aCoeff_k def * bCoeff_k def + 48 * aCoeff_k def ^ 2 * bCoeff_k def + 64 * aCoeff_k def ^ 3 * bCoeff_k def + 432 * bCoeff_k def ^ 2) == 0 = Just $ RefineOtherException (typeOf pred) (pack "The curve is singular")
    | not $ hasseTheorem (toInteger $ cofactor_k def) (toInteger $ order_k def) (2 ^ toInteger (degree_k def)) = Just $ RefineOtherException (typeOf pred) (pack "Hasse's theorem not satisfied")
    -- TODO check number of points exactly via Schoof algorithm or SEA algorithm
    | not $ koblitzEquationTest (toBinPoly modulus_k def) (toBinPoly aCoeff_k def) (toBinPoly bCoeff_k def) x y = Just $ RefineOtherException (typeOf pred) (pack "The generator doesn't lie on the curve")
    | otherwise = Nothing
    where x = fromIntegral $ fst . generator_k $ def
          y = fromIntegral $ snd . generator_k $ def

type RefinedKoblitzCurveDef' a = Refined KoblitzCurveDefValidator (UnrefinedKoblitzCurveDef a)
-- | A definition of the elliptic curve
newtype RefinedKoblitzCurveDef a = RefinedKoblitzCurveDef { unRefinedKoblitzCurveDef :: (RefinedKoblitzCurveDef' a)}


instance EllipticCurve RefinedStandardCurveDef Integer where
  aCoeff = refiner_s aCoeff_s
  bCoeff = refiner_s bCoeff_s
  generator = \def -> (refiner_s x_s def, refiner_s y_s def)
    where x_s = fst . generator_s
          y_s = snd . generator_s
  order = order_s . unrefine . unRefinedStandardCurveDef
  cofactor = cofactor_s . unrefine . unRefinedStandardCurveDef
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ refiner_s prime_s curve)
                            $ aCoeff curve

instance EllipticCurve RefinedKoblitzCurveDef F2Poly where
  aCoeff = refiner_k aCoeff_k
  bCoeff = refiner_k bCoeff_k
  generator = \def -> (refiner_k x_k def, refiner_k y_k def)
    where x_k = fst . generator_k
          y_k = snd . generator_k
  order = order_k . unrefine . unRefinedKoblitzCurveDef
  cofactor = cofactor_k . unrefine . unRefinedKoblitzCurveDef
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedKoblitzEllipticGroup
                            (reifiedGaloisBinaryField $ refiner_k modulus_k curve)
                            $ aCoeff curve


instance (Show a, Integral a) => Show (RefinedStandardCurveDef a) where
  show eC =
    "Galois Prime Field (p): " ++ show (refiner_s prime_s eC)
    ++ "\nCurve (a, b): y^2 = x^3 + " ++ show (aCoeff eC) ++ "*x + " ++ show (bCoeff eC)
    ++ "\nGenerator (G): " ++ show (generator eC)
    ++ "\nOrder (n): " ++ show (order eC)
    ++ "\nCofactor (h): " ++ show (cofactor eC)

instance (Show a, Integral a) => Show (RefinedKoblitzCurveDef a) where
  show eC =
    "Galois Binary Field (modulus): " ++ show (refiner_k modulus_k eC)
    ++ "\nCurve (a, b): y^2 + y*x = x^3 + " ++ show (aCoeff eC) ++ "*x^2 + " ++ show (bCoeff eC)
    ++ "\nGenerator (G): " ++ show (generator eC)
    ++ "\nOrder (n): " ++ show (order eC)
    ++ "\nCofactor (h): " ++ show (cofactor eC)