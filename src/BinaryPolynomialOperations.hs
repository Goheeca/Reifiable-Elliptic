module BinaryPolynomialOperations (
  polMult,
  polInv,
  polDiv
) where

import Data.Bit (F2Poly)


polDiv :: F2Poly -> F2Poly -> F2Poly -> (F2Poly, F2Poly)
polDiv _ n d = n `quotRem` d

polMult :: F2Poly -> F2Poly -> F2Poly -> F2Poly
polMult modulus a b = (a * b) `mod` modulus

polInv :: F2Poly -> F2Poly -> F2Poly
polInv modulus a = extendedEuclid 0 1 modulus a
  where extendedEuclid t newt r newr
          | newr == 0 = t
          | otherwise = extendedEuclid newt (t - polMult modulus quotient newt) newr remainder
          where (quotient, remainder) = polDiv modulus r newr
