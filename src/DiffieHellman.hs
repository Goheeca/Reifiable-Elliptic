-- | A collection of functions used in the Diffie-Hellman protocol.
module DiffieHellman (
  publicKey,
  sharedSecret,
  randomSecretKey
) where

import Elliptic
import ScalarMultiplication

import System.Random
import qualified NumericPrelude.Numeric as N (fromInteger)
import qualified Number.NonNegative as NN

-- | A computation of public key
publicKey :: (EllipticCurve a f b, Integral c)
          => a c                 -- ^ a curve definition
          -> NN.Integer          -- ^ a private key
          -> EllipticPoint f b   -- ^ a public key
publicKey curve secretKey = inElliptic curve $ generator curve +* secretKey

-- | A computation of shared secret
sharedSecret :: (EllipticCurve a f b, Integral c)
             => a c                  -- ^ a curve definition
             -> NN.Integer           -- ^ a private key
             -> EllipticPoint f b    -- ^ a public key of the other side
             -> EllipticPoint f b    -- ^ a shared secret
sharedSecret curve secretKey publicKey = inElliptic curve $ publicKey +* secretKey

-- | A random generator of private keys in the range suitable for the given curve
randomSecretKey :: (EllipticCurve a f b, Integral c)
                => a c                     -- ^ a curve definition
                -> IO NN.Integer           -- ^ a random private key
randomSecretKey curve = N.fromInteger <$> randomRIO (1, (toInteger . order $ curve) - 1)
