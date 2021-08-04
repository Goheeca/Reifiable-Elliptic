{-# LANGUAGE Rank2Types, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}

-- | An encapsulation of the computations in the reified algebraic structures.
module Elliptic (
  StandardCurveDef(..),
  KoblitzCurveDef(..),
  EllipticPoint,
  EllipticCurve(..),
  _binaryField
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
    rank_s :: NN.Integer,
    cofactor_s :: NN.Integer
  } deriving (Show, Eq)

-- | A definition of the elliptic curve
data KoblitzCurveDef a =
  KoblitzCurveDef {
    degree_k :: NN.Integer,
    modulus_k :: a,
    aCoeff_k :: a,
    bCoeff_k :: a,
    generator_k :: (a, a),
    rank_k :: NN.Integer,
    cofactor_k :: NN.Integer
  } deriving (Show, Eq)

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
  rank :: Integral c => a c -> NN.Integer
  cofactor :: Integral c => a c -> NN.Integer

toIngr :: Integral a => (b a -> a) -> b a -> Integer
toIngr fval def = fromIntegral $ fval def

instance EllipticCurve StandardCurveDef Integer where
  aCoeff = toIngr aCoeff_s
  bCoeff = toIngr bCoeff_s
  generator = \def -> (toIngr x_s def, toIngr y_s def)
    where x_s = fst . generator_s
          y_s = snd . generator_s
  rank = rank_s
  cofactor = cofactor_s
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ toIngr prime_s curve)
                            $ aCoeff curve

toBinPoly :: (Integral a, Integral b) => (KoblitzCurveDef a -> b) -> KoblitzCurveDef a -> F2Poly
toBinPoly fval def = fromInteger . toInteger $ fval def

modulus_BinPoly :: Integral a => KoblitzCurveDef a -> F2Poly
modulus_BinPoly def = fromInteger . toInteger $ modulus_k def

instance EllipticCurve KoblitzCurveDef F2Poly where
  aCoeff = toBinPoly aCoeff_k
  bCoeff = toBinPoly bCoeff_k
  generator = \def -> (toBinPoly x_k def, toBinPoly y_k def)
    where x_k = fst . generator_k
          y_k = snd . generator_k
  rank = rank_k
  cofactor = cofactor_k
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedKoblitzEllipticGroup
                            (reifiedGaloisBinaryField (toBinPoly degree_k curve) (modulus_BinPoly curve))
                            $ aCoeff curve

_binaryField curve = reifiedGaloisBinaryField (toBinPoly degree_k curve) (modulus_BinPoly curve)
