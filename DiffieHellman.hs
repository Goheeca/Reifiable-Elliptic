module DiffieHellman (
  publicKey,
  sharedSecret,
  randomSecretKey
) where

import Elliptic
import PowerOperation

import System.Random
import Control.Monad (liftM)
import qualified NumericPrelude.Numeric as N (fromInteger)
import qualified Number.NonNegative as NN

publicKey :: CurveDef Integer -> NN.Integer -> (Integer, Integer)
publicKey curve secretKey = inElliptic curve $ generator curve +^ secretKey

sharedSecret :: CurveDef Integer -> NN.Integer -> (Integer, Integer) -> (Integer, Integer)
sharedSecret curve secretKey publicKey = inElliptic curve $ publicKey +^ secretKey

randomSecretKey :: CurveDef Integer -> IO NN.Integer
randomSecretKey curve = liftM N.fromInteger $ randomRIO (1, (rank curve) - 1)
