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
polInv p a = polInv_ 0 1 p a
  where polInv_ t nt r nr
          | nr == 0   = t
          | otherwise =
            let (quotient, remainder) = polDiv p r nr
            in polInv_ nt (t + (polMult p quotient nt)) nr remainder
