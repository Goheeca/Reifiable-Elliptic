{-# LANGUAGE Rank2Types, FlexibleContexts, FunctionalDependencies #-}

-- | An encapsulation of the computations in the reified algebraic structures.
module Elliptic (
  StandardCurveDef(..),
  KoblitzCurveDef(..),
  EllipticPoint,
  EllipticCurve(..),
  F2Poly,
) where

import ReifiedElliptic

import qualified Algebra.Additive as AbelianGroup
import Data.Bit (F2Poly)
import qualified Number.NonNegative as NN

import Reificator (using)

-- | A definition of the elliptic curve
data StandardCurveDef a =
  StandardCurveDef {
    prime_s :: a,
    aCoeff_s :: a,
    bCoeff_s :: a,
    generator_s :: (a, a),
    order_s :: NN.Integer,
    cofactor_s :: NN.Integer
  }

-- | A definition of the elliptic curve
data KoblitzCurveDef a =
  KoblitzCurveDef {
    degree_k :: NN.Integer,
    modulus_k :: a,
    aCoeff_k :: a,
    bCoeff_k :: a,
    generator_k :: (a, a),
    order_k :: NN.Integer,
    cofactor_k :: NN.Integer
  }

class EllipticCurve a b | a -> b where
  -- | For computations in an elliptic group
  inElliptic :: Integral c
             => a c
             -> ((AbelianGroup.C (EllipticPoint b))
                 => EllipticPoint b)
             -> EllipticPoint b
  aCoeff :: Integral c => a c -> b
  bCoeff :: Integral c => a c -> b
  generator :: Integral c => a c -> (b, b)
  order :: Integral c => a c -> NN.Integer
  cofactor :: Integral c => a c -> NN.Integer

toIntgr :: Integral a => (b a -> a) -> b a -> Integer
toIntgr fval def = fromIntegral $ fval def

instance EllipticCurve StandardCurveDef Integer where
  aCoeff = toIntgr aCoeff_s
  bCoeff = toIntgr bCoeff_s
  generator = \def -> (toIntgr x_s def, toIntgr y_s def)
    where x_s = fst . generator_s
          y_s = snd . generator_s
  order = order_s
  cofactor = cofactor_s
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ toIntgr prime_s curve)
                            $ aCoeff curve

instance (Show a, Integral a) => Show (StandardCurveDef a) where
  show eC =
    "Galois Prime Field (p): " ++ show (prime_s eC)
    ++ "\nCurve (a, b): y^2 = x^3 + " ++ show (aCoeff eC) ++ "*x + " ++ show (bCoeff eC)
    ++ "\nGenerator (G): " ++ show (generator eC)
    ++ "\nOrder (n): " ++ show (order eC)
    ++ "\nCofactor (h): " ++ show (cofactor eC)


toBinPoly :: (Integral a, Integral b) => (KoblitzCurveDef a -> b) -> KoblitzCurveDef a -> F2Poly
toBinPoly fval def = fromInteger . toInteger $ fval def

instance EllipticCurve KoblitzCurveDef F2Poly where
  aCoeff = toBinPoly aCoeff_k
  bCoeff = toBinPoly bCoeff_k
  generator = \def -> (toBinPoly x_k def, toBinPoly y_k def)
    where x_k = fst . generator_k
          y_k = snd . generator_k
  order = order_k
  cofactor = cofactor_k
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedKoblitzEllipticGroup
                            (reifiedGaloisBinaryField $ toBinPoly modulus_k curve)
                            $ aCoeff curve

instance (Show a, Integral a) => Show (KoblitzCurveDef a) where
  show eC =
    "Galois Binary Field (modulus): " ++ show (modulus_k eC)
    ++ "\nCurve (a, b): y^2 + y*x = x^3 + " ++ show (aCoeff eC) ++ "*x^2 + " ++ show (bCoeff eC)
    ++ "\nGenerator (G): " ++ show (generator eC)
    ++ "\nOrder (n): " ++ show (order eC)
    ++ "\nCofactor (h): " ++ show (cofactor eC)

