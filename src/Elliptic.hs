{-# LANGUAGE Rank2Types, FlexibleContexts, FunctionalDependencies #-}

-- | An encapsulation of the computations in the reified algebraic structures.
module Elliptic (
  EllipticPoint,
  EllipticCurve(..),
) where

import ReifiedElliptic (EllipticPoint)

import qualified Algebra.Additive as AbelianGroup
import qualified Number.NonNegative as NN


class EllipticCurve a f b | a -> b, a -> f where
  -- | For computations in an elliptic group
  inElliptic :: Integral c
             => a c
             -> ((AbelianGroup.C (EllipticPoint f b))
                 => EllipticPoint f b)
             -> EllipticPoint f b
  generator :: Integral c => a c -> EllipticPoint f b
  order :: Integral c => a c -> NN.Integer
  cofactor :: Integral c => a c -> NN.Integer