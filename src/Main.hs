{-# LANGUAGE TemplateHaskell, TypeApplications, FlexibleContexts #-}

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
useCurve :: (EllipticCurve a f b, Show b, Integral c, Show (a c), Show (EllipticPoint f b)) => a c -> IO ()
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
    (2^192-2^64-1)
    (-3)
    0x64210519e59c80e70fa7e9ab72243049feb8deecc146b9b1
    (0x188da80eb03090f67cbf20eb43a18800f4ff0afd82ff1012, 0x07192b95ffc8da78631011ed6b24cdd573f977a11e794811)
    6277101735386680763835789423176059013767194773182842284081
    1
  )

-- | The standard NIST curve P-224
curveP224 :: RefinedStandardCurveDef Integer
curveP224 = RefinedStandardCurveDef
  $$(refineTH $ UnrefinedStandardCurveDef @Integer
    (2^224-2^96+1)
    (-3)
    0xb4050a850c04b3abf54132565044b0b7d7bfd8ba270b39432355ffb4
    (0xb70e0cbd6bb4bf7f321390b94a03c1d356c21122343280d6115c1d21, 0xbd376388b5f723fb4c22dfe6cd4375a05a07476444d5819985007e34)
    26959946667150639794667015087019625940457807714424391721682722368061
    1
  )

-- | The standard NIST curve P-256
curveP256 :: RefinedStandardCurveDef Integer
curveP256 = RefinedStandardCurveDef
  $$(refineTH $ UnrefinedStandardCurveDef @Integer
    (2^256-2^224+2^192+2^96-1)
    (-3)
    0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b
    (0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296, 0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5)
    115792089210356248762697446949407573529996955224135760342422259061068512044369
    1
  )

-- | The standard NIST curve P-384
curveP384 :: RefinedStandardCurveDef Integer
curveP384 = RefinedStandardCurveDef
  $$(refineTH $ UnrefinedStandardCurveDef @Integer
    (2^384-2^128-2^96+2^32-1)
    (-3)
    0xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef
    (0xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7, 0x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f)
    39402006196394479212279040100143613805079739270465446667946905279627659399113263569398956308152294913554433653942643
    1
  )

-- | The standard NIST curve P-521
curveP521 :: RefinedStandardCurveDef Integer
curveP521 = RefinedStandardCurveDef
  $$(refineTH $ UnrefinedStandardCurveDef @Integer
    (2^521-1)
    (-3)
    0x051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00
    (0xc6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66, 0x11839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650)
    6864797660130609714981900799081393217269435300143305409394463459185543183397655394245057746333217197532963996371363321113864768612440380340372808892707005449
    1
  )

-- | The Koblitz NIST curve K-163
curveK163 :: RefinedKoblitzCurveDef F2Poly
curveK163 = RefinedKoblitzCurveDef
  $$(refineTH $ UnrefinedKoblitzCurveDef @F2Poly
    163
    (2^163+2^7+2^6+2^3+2^0)
    1
    1
    (0x2fe13c0537bbc11acaa07d793de4e6d5e5c94eee8, 0x289070fb05d38ff58321f2e800536d538ccdaa3d9)
    5846006549323611672814741753598448348329118574063
    2
  )

-- | The Koblitz NIST curve K-233
curveK233 :: RefinedKoblitzCurveDef F2Poly
curveK233 = RefinedKoblitzCurveDef
  $$(refineTH $ UnrefinedKoblitzCurveDef @F2Poly
    233
    (2^233+2^74+2^0)
    0
    1
    (0x17232ba853a7e731af129f22ff4149563a419c26bf50a4c9d6eefad6126, 0x1db537dece819b7f70f555a67c427a8cd9bf18aeb9b56e0c11056fae6a3)
    3450873173395281893717377931138512760570940988862252126328087024741343
    4
  )

-- | The Koblitz NIST curve K-283
curveK283 :: RefinedKoblitzCurveDef F2Poly
curveK283 = RefinedKoblitzCurveDef
  $$(refineTH $ UnrefinedKoblitzCurveDef @F2Poly
    283
    (2^283+2^12+2^7+2^5+2^0)
    0
    1
    (0x503213f78ca44883f1a3b8162f188e553cd265f23c1567a16876913b0c2ac2458492836, 0x1ccda380f1c9e318d90f95d07e5426fe87e45c0e8184698e45962364e34116177dd2259)
    3885337784451458141838923813647037813284811733793061324295874997529815829704422603873
    4
  )

-- | The Koblitz NIST curve K-409
curveK409 :: RefinedKoblitzCurveDef F2Poly
curveK409 = RefinedKoblitzCurveDef
  $$(refineTH $ UnrefinedKoblitzCurveDef @F2Poly
    409
    (2^409+2^87+2^0)
    0
    1
    (0x060f05f658f49c1ad3ab1890f7184210efd0987e307c84c27accfb8f9f67cc2c460189eb5aaaa62ee222eb1b35540cfe9023746, 0x1e369050b7c4e42acba1dacbf04299c3460782f918ea427e6325165e9ea10e3da5f6c42e9c55215aa9ca27a5863ec48d8e0286b)
    330527984395124299475957654016385519914202341482140609642324395022880711289249191050673258457777458014096366590617731358671
    4
  )

-- | The Koblitz NIST curve K-571
curveK571 :: RefinedKoblitzCurveDef F2Poly
curveK571 = RefinedKoblitzCurveDef
  $$(refineTH $ UnrefinedKoblitzCurveDef @F2Poly
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
