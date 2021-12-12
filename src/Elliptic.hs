{-# LANGUAGE FunctionalDependencies #-}

-- | An encapsulation of the computations in the reified algebraic structures.
module Elliptic (
  EllipticPoint,
  EllipticCurve(..),
) where

import ReifiedElliptic (EllipticPoint)

import qualified Number.NonNegative as NN


class EllipticCurve a f b | a -> b, a -> f where
  -- | For computations in an elliptic group
  scalarMultiplication :: Integral c
                       => a c
                       -> EllipticPoint f b
                       -> NN.Integer
                       -> EllipticPoint f b
  generator :: Integral c => a c -> EllipticPoint f b
  order :: Integral c => a c -> NN.Integer
  cofactor :: Integral c => a c -> NN.Integer