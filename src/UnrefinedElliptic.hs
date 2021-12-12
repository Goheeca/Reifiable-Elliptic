{-# LANGUAGE TypeApplications, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveLift, TemplateHaskellQuotes #-}

module UnrefinedElliptic (
  UnrefinedStandardCurveDef(..),
  UnrefinedKoblitzCurveDef(..),
  UnrefinedMontgomeryCurveDef(..),
) where

import Number.NonNegative as NN
import Language.Haskell.TH.Syntax
import Data.Bit (F2Poly(..))

-- | A possible definition of the elliptic curve
data UnrefinedStandardCurveDef a =
  UnrefinedStandardCurveDef {
    prime_s :: a,
    aCoeff_s :: a,
    bCoeff_s :: a,
    generator_s :: (a, a),
    order_s :: NN.Integer,
    cofactor_s :: NN.Integer
  }
  deriving Lift

-- | A possible definition of the elliptic curve
data UnrefinedKoblitzCurveDef a =
  UnrefinedKoblitzCurveDef {
    degree_k :: NN.Integer,
    modulus_k :: a,
    aCoeff_k :: a,
    bCoeff_k :: a,
    generator_k :: (a, a),
    order_k :: NN.Integer,
    cofactor_k :: NN.Integer
  }
  deriving Lift

data UnrefinedMontgomeryCurveDef a =
  UnrefinedMontgomeryCurveDef {
    prime_m :: a,
    aCoeff_m :: a,
    bCoeff_m :: a,
    generator_m :: (a, a),
    order_m :: NN.Integer,
    cofactor_m :: NN.Integer
  }
  deriving Lift


instance Lift NN.Integer where
  liftTyped nnInteger = [|| fromInteger @NN.Integer nnIntegerInteger ||]
    where nnIntegerInteger = toInteger nnInteger
  lift nnInteger = [| fromInteger @NN.Integer nnIntegerInteger |]
    where nnIntegerInteger = toInteger nnInteger

instance Lift F2Poly where
  liftTyped f2poly = [|| fromInteger @F2Poly f2polyInt ||]
    where f2polyInt = toInteger f2poly
  lift f2poly = [| fromInteger @F2Poly f2polyInt |]
    where f2polyInt = toInteger f2poly