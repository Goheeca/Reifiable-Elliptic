module BinaryPolynomialOperations (
  polMult,
  polInv,
  polDiv
) where

--import Data.Bits
import Data.BitVector

fieldDegree :: BV -> Int
fieldDegree = flip (-) 1 . size

polDiv :: BV -> BV -> BV -> (BV, BV)
polDiv m n d
  | n < d     = (bitVec (fieldDegree m) 0, bitVec (fieldDegree m) n)
  | otherwise =
      let subResult = polDiv m (n `xor` (d # zeros diffLen)) d
      in (setBit (fst subResult) diffLen, snd subResult)
  where len = fromInteger . nat . lg2
        diffLen = len n - len d

polMult :: BV -> BV -> BV -> BV
polMult modulus a b = bitVec degree $ mult_ degree 0 a b
  where degree = fieldDegree modulus
        mult_ 0 p _ _ = p
        mult_ n p a b =
          let shiftA = a # zeros 1
          in mult_ (n - 1)
                   (if testBit b 0 then p `xor` a else p)
                   (if testBit shiftA degree then shiftA `xor` modulus else shiftA)
                   (clearBit (b >>. 1) degree)

polInv :: BV -> BV -> BV
polInv p a = polInv_ 0 1 p a
  where polInv_ t nt r nr
          | nr == 0   = t
          | otherwise =
            let (quotient, remainder) = polDiv p r nr
            in polInv_ nt (t `xor` (polMult p quotient nt)) nr remainder
