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
import Number.NonNegative (fromNumber)
import Data.Typeable (Typeable, typeOf)
import Data.Text (pack)
import Data.Bit (F2Poly)


data StandardCurveDefValidator

instance (Typeable a, Integral a) => Predicate StandardCurveDefValidator (UnrefinedStandardCurveDef a) where
  validate _ def
    | not $ isCertifiedPrime . toInteger $ prime_s def = Just $ RefineOtherException (typeOf def) (pack "Modulus isn't prime")
    | 0 >= (toInteger $ cofactor_s def) = Just $ RefineOtherException (typeOf def) (pack "Cofactor isn't positive")
    | otherwise = Nothing

type RefinedStandardCurveDef' a = Refined StandardCurveDefValidator (UnrefinedStandardCurveDef a)
-- | A definition of the elliptic curve
newtype RefinedStandardCurveDef a = RefinedStandardCurveDef { unRefinedStandardCurveDef :: (RefinedStandardCurveDef' a)}


data KoblitzCurveDefValidator

instance (Typeable a, Integral a) => Predicate KoblitzCurveDefValidator (UnrefinedKoblitzCurveDef a) where
  validate _ def = Nothing

type RefinedKoblitzCurveDef' a = Refined KoblitzCurveDefValidator (UnrefinedKoblitzCurveDef a)
-- | A definition of the elliptic curve
newtype RefinedKoblitzCurveDef a = RefinedKoblitzCurveDef { unRefinedKoblitzCurveDef :: (RefinedKoblitzCurveDef' a)}


toIntgr :: Integral a => (b a -> a) -> b a -> Integer
toIntgr fval def = fromIntegral $ fval def

refiner_s :: Integral a => (UnrefinedStandardCurveDef a -> a) -> RefinedStandardCurveDef a -> Integer
refiner_s fval = toIntgr $ fval . unrefine . unRefinedStandardCurveDef

instance EllipticCurve RefinedStandardCurveDef Integer where
  aCoeff = refiner_s aCoeff_s
  bCoeff = refiner_s bCoeff_s
  generator = \def -> (refiner_s x_s def, refiner_s y_s def)
    where x_s = fst . generator_s
          y_s = snd . generator_s
  order = fromNumber . toInteger . order_s . unrefine . unRefinedStandardCurveDef
  cofactor = fromNumber . toInteger . cofactor_s . unrefine . unRefinedStandardCurveDef
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ refiner_s prime_s curve)
                            $ aCoeff curve

toBinPoly :: (Integral a, Integral b) => (c a -> b) -> c a -> F2Poly
toBinPoly fval def = fromInteger . toInteger $ fval def

refiner_k :: (Integral a, Integral b) => (UnrefinedKoblitzCurveDef a -> b) -> RefinedKoblitzCurveDef a -> F2Poly
refiner_k fval = toBinPoly $ fval . unrefine . unRefinedKoblitzCurveDef

instance EllipticCurve RefinedKoblitzCurveDef F2Poly where
  aCoeff = refiner_k aCoeff_k
  bCoeff = refiner_k bCoeff_k
  generator = \def -> (refiner_k x_k def, refiner_k y_k def)
    where x_k = fst . generator_k
          y_k = snd . generator_k
  order = fromNumber . toInteger . order_k . unrefine . unRefinedKoblitzCurveDef
  cofactor = fromNumber . toInteger . cofactor_k . unrefine . unRefinedKoblitzCurveDef
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