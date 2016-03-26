module BinaryPolynomialOperations (
  polMult,
  polInv
) where

import Data.Bits

intLen :: Integer -> Int
intLen 0 = 0
intLen n = 1 + intLen (shift n (-1))

polDiv :: Integer -> Integer -> (Integer, Integer)
polDiv n d
  | n < d     = (0, n)
  | otherwise =
    let subResult = polDiv (n `xor` (shift d diffLen)) d
    in (setBit (fst subResult) diffLen, snd subResult)
  where diffLen = intLen n - intLen d

polMult :: Integer -> Integer -> Integer -> Integer
polMult modulus a b = mult_ degree 0 a b
  where degree = intLen modulus - 1 
        mult_ 0 p _ _ = p
        mult_ n p a b =
          let shiftA = shift a 1
          in mult_ (n - 1)
                   (if testBit b 0 then p `xor` a else p)
                   (if testBit shiftA degree then shiftA `xor` modulus else shiftA)
                   (clearBit (shift b (-1)) degree)

polInv :: Integer -> Integer -> Integer
polInv p a = polInv_ 0 1 p a
  where polInv_ t nt r nr
          | nr == 0   = t
          | otherwise =
            let (quotient, remainder) = polDiv r nr
            in polInv_ nt (t `xor` (polMult p quotient nt)) nr remainder
