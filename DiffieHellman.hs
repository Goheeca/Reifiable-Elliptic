-- | A collection of functions used in the Diffie-Hellman protocol.
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

-- | A computation of public key
publicKey :: CurveDef Integer         -- ^ a curve definition
          -> NN.Integer               -- ^ a private key
          -> Maybe (Integer, Integer) -- ^ a public key
publicKey curve secretKey = inElliptic curve $ Just (generator curve) +* secretKey

-- | A computation of shared secret
sharedSecret :: CurveDef Integer         -- ^ a curve definition
             -> NN.Integer               -- ^ a private key
             -> Maybe (Integer, Integer) -- ^ a public key of the other side
             -> Maybe (Integer, Integer) -- ^ a shared secret
sharedSecret curve secretKey publicKey = inElliptic curve $ publicKey +* secretKey

-- | A random generator of private keys in the range suitable for the given curve
randomSecretKey :: CurveDef Integer -- ^ a curve definition
                -> IO NN.Integer    -- ^ a random private key
randomSecretKey curve = liftM N.fromInteger $ randomRIO (1, (rank curve) - 1)
