{-# LANGUAGE Rank2Types, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}

-- | An encapsulation of the computations in the reified algebraic structures.
module Elliptic (
  StandardCurveDef(..),
  KoblitzCurveDef(..),
  EllipticPoint,
  EllipticCurve(..)
) where

import ReifiedElliptic

import qualified Algebra.Additive as AbelianGroup
import Data.BitVector

import Reificator (using)

-- | A definition of the elliptic curve
data StandardCurveDef a =
  StandardCurveDef {
    prime_s :: a,
    aCoeff_s :: a,
    bCoeff_s :: a,
    generator_s :: (a, a),
    rank_s :: a,
    cofactor_s :: a
  } deriving (Show, Eq)

-- | A definition of the elliptic curve
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

class EllipticCurve a b | a -> b where
  -- | For computations in an elliptic group
  inElliptic :: a c
              -> ((AbelianGroup.C (EllipticPoint b))
                  => EllipticPoint b)
              -> EllipticPoint b
  aCoeff :: a c -> b
  bCoeff :: a c -> b
  generator :: a c -> (b, b)
  rank :: a c -> b
  cofactor :: a c -> b

instance EllipticCurve StandardCurveDef Integer where
  aCoeff = fromInteger . aCoeff_s
  bCoeff = fromInteger . bCoeff_s
  generator = fromInteger . generator_s
  rank = fromInteger . rank_s
  cofactor = fromInteger . cofactor_s
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ prime_s curve)
                            $ aCoeff curve

toBV :: Integral a => (KoblitzCurveDef a -> a) -> KoblitzCurveDef a -> BV
toBV fval def = bitVec ((size $ fromIntegral $ modulus_k def) - 1) $ fval def

instance EllipticCurve KoblitzCurveDef BV where
  aCoeff = toBV aCoeff_k
  bCoeff = toBV bCoeff_k
  generator = \def -> (toBV x_k def, toBV y_k def)
    where x_k = fst . generator_k
          y_k = snd . generator_k
  rank = toBV rank_k
  cofactor = toBV cofactor_k
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedKoblitzEllipticGroup
                            (reifiedGaloisBinaryField (degree_k curve) (modulus_k curve))
                            $ aCoeff curve
