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
  inElliptic :: Integral c
             => a c
             -> ((AbelianGroup.C (EllipticPoint b))
                 => EllipticPoint b)
             -> EllipticPoint b
  aCoeff :: Integral c => a c -> b
  bCoeff :: Integral c => a c -> b
  generator :: Integral c => a c -> (b, b)
  rank :: Integral c => a c -> b
  cofactor :: Integral c => a c -> b

toIngr :: Integral a => (b a -> a) -> b a -> Integer
toIngr fval def = fromIntegral $ fval def

instance EllipticCurve StandardCurveDef Integer where
  aCoeff = toIngr aCoeff_s
  bCoeff = toIngr bCoeff_s
  generator = \def -> (toIngr x_s def, toIngr y_s def)
    where x_s = fst . generator_s
          y_s = snd . generator_s
  rank = toIngr rank_s
  cofactor = toIngr cofactor_s
  inElliptic curve = using ellipticGroup
    where ellipticGroup = reifiedStandardEllipticGroup
                            (reifiedGaloisPrimeField $ toIngr prime_s curve)
                            $ aCoeff curve

toBV :: Integral a => (KoblitzCurveDef a -> a) -> KoblitzCurveDef a -> BV
toBV fval def = bitVec ((size $ fromIntegral $ modulus_k def) - 1) $ fval def

modulus_BV :: Integral a => KoblitzCurveDef a -> BV
modulus_BV def = bitVec (size $ fromIntegral $ modulus_k def) $ modulus_k def

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
                            (reifiedGaloisBinaryField (toBV degree_k curve) (modulus_BV curve))
                            $ aCoeff curve

_binaryField curve = reifiedGaloisBinaryField (toBV degree_k curve) (modulus_BV curve)
