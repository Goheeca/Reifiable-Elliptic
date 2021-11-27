{-# LANGUAGE DeriveLift #-}

module UnrefinedElliptic (
  UnrefinedStandardCurveDef(..),
  UnrefinedKoblitzCurveDef(..),
) where

import Language.Haskell.TH.Syntax


-- | A possible definition of the elliptic curve
data UnrefinedStandardCurveDef a =
  UnrefinedStandardCurveDef {
    prime_s :: a,
    aCoeff_s :: a,
    bCoeff_s :: a,
    generator_s :: (a, a),
    rank_s :: a,
    cofactor_s :: a
  }
  deriving Lift

-- | A possible definition of the elliptic curve
data UnrefinedKoblitzCurveDef a =
  UnrefinedKoblitzCurveDef {
    degree_k :: a,
    modulus_k :: a,
    aCoeff_k :: a,
    bCoeff_k :: a,
    generator_k :: (a, a),
    rank_k :: a,
    cofactor_k :: a
  }
  deriving Lift
