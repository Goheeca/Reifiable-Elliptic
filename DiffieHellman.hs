{-# LANGUAGE FlexibleContexts #-}

module DiffieHellman (
  publicKey,
  sharedSecret,
  randomSecretKey
) where

import Elliptic
import ScalarMultiplication

import System.Random
import Control.Monad (liftM)
import Data.Maybe
import qualified NumericPrelude.Numeric as N (fromInteger)
import qualified Number.NonNegative as NN

publicKey :: EllipticCurve a b => a b -> NN.Integer -> EllipticPoint b
publicKey curve secretKey = inElliptic curve $ Just (generator curve) +* secretKey

sharedSecret :: EllipticCurve a b => a b -> NN.Integer -> EllipticPoint b -> EllipticPoint b
sharedSecret curve secretKey publicKey = inElliptic curve $ publicKey +* secretKey

randomSecretKey :: EllipticCurve a Integer => a Integer -> IO NN.Integer
randomSecretKey curve = liftM N.fromInteger $ randomRIO (1, (rank curve) - 1)
