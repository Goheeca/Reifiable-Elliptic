{-# LANGUAGE TemplateHaskell, TypeApplications #-}

{- | The following simple example incorporates curve defined in <http://crypto-world.info/klima/2002/chip-2002-09-134-136.pdf this article> (czech only).

The example itself:

>>> useCurve curveTest
Galois Prime Field (p): 23
Curve (a, b): y^2 = x^3 + 1*x + 1
Generator (G): (13,16)
Order (n): 7
Cofactor (h): 4
<BLANKLINE>
Alice
SECRET: 3
PUBLIC: Just (17,20)
<BLANKLINE>
Bob
SECRET: 4
PUBLIC: Just (17,3)
<BLANKLINE>
SHARED
Alice: Just (5,4)
Bob: Just (5,4)
-}
module Main where

import Elliptic
import DiffieHellman
import UnrefinedElliptic
import RefinedElliptic
import Refined (refineTH)

-- | Does a Diffie-Hellman exchange with the elliptic curve
useCurve :: (EllipticCurve a b, Show b, Integral c, Show (a c)) => a c -> IO ()
useCurve eC = do
  aliceSecret <- randomSecretKey eC
  bobSecret <- randomSecretKey eC
  let alicePublic = publicKey eC aliceSecret
  let bobPublic = publicKey eC bobSecret
  let aliceSharedSecret = sharedSecret eC aliceSecret bobPublic
  let bobSharedSecret = sharedSecret eC bobSecret alicePublic
  print eC
  putStrLn ""
  putStrLn "Alice"
  putStrLn $ "SECRET: " ++ show aliceSecret
  putStrLn $ "PUBLIC: " ++ show alicePublic
  putStrLn ""
  putStrLn "Bob"
  putStrLn $ "SECRET: " ++ show bobSecret
  putStrLn $ "PUBLIC: " ++ show bobPublic
  putStrLn ""
  putStrLn "SHARED"
  putStrLn $ "Alice: " ++ show aliceSharedSecret
  putStrLn $ "Bob: " ++ show bobSharedSecret

-- | The example of a small curve from the aforementioned hyperlink
curveTest :: RefinedStandardCurveDef Integer
curveTest = RefinedStandardCurveDef $$(refineTH $ UnrefinedStandardCurveDef @Integer 23 1 1 (13, 16) 7 4)

-- | The standard NIST curve P-192
curveP192 :: RefinedStandardCurveDef Integer
curveP192 = RefinedStandardCurveDef
  $$(refineTH $ UnrefinedStandardCurveDef @Integer
    6277101735386680763835789423207666416083908700390324961279
    (-3)
    0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
    (0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012, 0x07192b95ffc8da78631011ed6b24cdd573f977a11e794811)
    6277101735386680763835789423176059013767194773182842284081
    1
  )

-- | The standard NIST curve P-521
curveP521 :: RefinedStandardCurveDef Integer
curveP521 = RefinedStandardCurveDef
  $$(refineTH $ UnrefinedStandardCurveDef @Integer
    6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151
    (-3)
    0x051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00
    (0xc6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66, 0x11839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650)
    6864797660130609714981900799081393217269435300143305409394463459185543183397655394245057746333217197532963996371363321113864768612440380340372808892707005449
    1
  )

-- | The Koblitz NIST curve K-571
curveK571 :: RefinedKoblitzCurveDef Integer
curveK571 = RefinedKoblitzCurveDef
  $$(refineTH $ UnrefinedKoblitzCurveDef @Integer
    571
    (2^571+2^10+2^5+2^2+2^0)
    0
    1
    (0x26eb7a859923fbc82189631f8103fe4ac9ca2970012d5d46024804801841ca44370958493b205e647da304db4ceb08cbbd1ba39494776fb988b47174dca88c7e2945283a01c8972, 0x349dc807f4fbf374f4aeade3bca95314dd58cec9f307a54ffc61efc006d8a2c9d4979c0ac44aea74fbebbb9f772aedcb620b01a7ba7af1b320430c8591984f601cd4c143ef1c7a3)
    1932268761508629172347675945465993672149463664853217499328617625725759571144780212268133978522706711834706712800825351461273674974066617311929682421617092503555733685276673
    4
  )

-- | Runs the example with P-521 and K-571
main :: IO ()
main = do
  putStrLn "Curve P-521" >> putStrLn ""
  useCurve curveP521
  putStrLn "" >> putStrLn "Curve K-571" >> putStrLn ""
  useCurve curveK571
