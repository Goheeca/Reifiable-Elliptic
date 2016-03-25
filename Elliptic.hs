{-# LANGUAGE Rank2Types, FlexibleContexts, MultiParamTypeClasses #-}

module Elliptic (
  StandardCurveDef(..),
  KoblitzCurveDef(..),
  EllipticPoint,
  EllipticCurve(..)
) where

import ReifiedElliptic

import qualified Algebra.Additive as AbelianGroup

import Reificator (using)

data StandardCurveDef a =
  StandardCurveDef {
    prime_s :: a,
    aCoeff_s :: a,
    bCoeff_s :: a,
    generator_s :: (a, a),
    rank_s :: a,
    cofactor_s :: a
  } deriving (Show, Eq)

data KoblitzCurveDef a =
  KoblitzCurveDef {
    degree_k :: a,
    modulus_k :: a,
    aCoeff_k :: a,
    bCoeff_k :: a,
    generator_k :: (a, a),
    rank_k :: a,
    cofactor_k :: a
  } deriving (Show, Eq)

class EllipticCurve a b where
  inElliptic :: a b
              -> ((AbelianGroup.C (EllipticPoint b))
                  => EllipticPoint b)
              -> EllipticPoint b
  aCoeff :: a b -> b
  bCoeff :: a b -> b
  generator :: a b -> (b, b)
  rank :: a b -> b
  cofactor :: a b -> b

instance EllipticCurve StandardCurveDef Integer where
  aCoeff = aCoeff_s
  bCoeff = bCoeff_s
  generator = generator_s
  rank = rank_s
  cofactor = cofactor_s
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ prime_s curve)
                            $ aCoeff curve

instance EllipticCurve KoblitzCurveDef Integer where
  aCoeff = aCoeff_k
  bCoeff = bCoeff_k
  generator = generator_k
  rank = rank_k
  cofactor = cofactor_k
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedKoblitzEllipticGroup
                            (reifiedGaloisBinaryField (degree_k curve) (modulus_k curve))
                            $ aCoeff curve
