{-# LANGUAGE Rank2Types, FlexibleContexts #-}

module Elliptic (
  CurveDef(..),
  inElliptic
) where

import ReifiedElliptic

import qualified Algebra.Additive as AbelianGroup

import Reificator (using)

data CurveDef a =
  CurveDef {
    prime :: a,
    aCoeff :: a,
    bCoeff :: a,
    generator :: (a, a),
    rank :: a,
    cofactor :: a
  } deriving (Show, Eq)

inElliptic :: CurveDef Integer
              -> ((AbelianGroup.C (Maybe (Integer, Integer)))
                  => Maybe (Integer, Integer))
              -> Maybe (Integer, Integer)
inElliptic curve = using ellipticGroup
  where ellipticGroup = reifiedEllipticGroup
                          (reifiedGaloisPrimeField $ prime curve)
                          $ aCoeff curve
