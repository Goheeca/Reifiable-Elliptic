{-# LANGUAGE Rank2Types, FlexibleContexts #-}

-- | An encapsulation of the computations in the reified algebraic structures.
module Elliptic (
  CurveDef(..),
  inElliptic
) where

import ReifiedElliptic

import qualified Algebra.Additive as AbelianGroup

import Reificator (using)

-- | A definition of the elliptic curve
data CurveDef a =
  CurveDef {
    prime :: a,
    aCoeff :: a,
    bCoeff :: a,
    generator :: (a, a),
    rank :: a,
    cofactor :: a
  } deriving (Show, Eq)

-- | For computations in an elliptic group
inElliptic :: CurveDef Integer                                -- ^ a definition of the curve
              -> ((AbelianGroup.C (Maybe (Integer, Integer)))
                  => Maybe (Integer, Integer))                -- ^ a computation over the elliptic group
              -> Maybe (Integer, Integer)                     -- ^ the resulting elliptic point
inElliptic curve = using ellipticGroup
  where ellipticGroup = reifiedEllipticGroup
                          (reifiedGaloisPrimeField $ prime curve)
                          $ aCoeff curve
