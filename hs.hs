doble x = x + x

triple x = x*3

-----------------------------------

absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | otherwise = -x

-----------------------------------

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | (absoluto x) >= (absoluto y) = absoluto x
                   | otherwise = absoluto y

-----------------------------------

maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c | a >= b && a >= c = a 
              | b >= c = b
              | otherwise = c

-----------------------------------

algunoEs0 :: Rational -> Rational -> Bool
algunoEs0 x y | x == 0 || y == 0 = True
              | otherwise        = False

algunoEs0bis :: Rational -> Rational -> Bool
algunoEs0bis 0 _ = True
algunoEs0bis _ 0 = True
algunoEs0bis _ _ = False

-----------------------------------

ambosSon0 :: Rational -> Rational -> Bool
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise        = False

ambosSon0bis :: Rational -> Rational -> Bool
ambosSon0bis 0 0 = True
ambosSon0bis _ _ = False

-----------------------------------

sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x == y && x == z = 0
                    | x == y = z
                    | y == z = x
                    | x == z = y
                    | otherwise = x + y + z

-----------------------------------

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod a b == 0

-----------------------------------

digitoUnidades :: Int -> Int
digitoUnidades n = mod (absoluto n) 10

-----------------------------------

digitoDecenas :: Int -> Int
digitoDecenas n = mod (div n 10) 10

-----------------------------------

--4

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a, b) (c, d) = a < c && b < d

-------------

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (a, b) (c, d) = a*c+b*d

-------------

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a, b) (c, d) = sqrt((c-a)^2 + (d-b)^2)

-------------

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

-------------

sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (x, y, z) n | mod x n == 0 && mod y n == 0 && mod z n == 0 = x + y + z
                               | mod x n == 0 && mod y n == 0 = x + y
                               | mod y n == 0 && mod z n == 0 = y + z
                               | mod x n == 0 && mod z n == 0 = x + z
                               | mod x n == 0 = x
                               | mod y n == 0 = y
                               | mod z n == 0 = z
                               | otherwise = 0

sumarSoloMultiplos2 :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos2 (x, y, z) n = getMultiplo n x + getMultiplo n y + getMultiplo n z

getMultiplo :: Int -> Int -> Int
getMultiplo a b
  | mod b a == 0 = b
  | otherwise = 0

-------------

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (x, y, z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4

-------------

crearPar :: a -> b -> (a,b)
crearPar a b = (a,b)

-------------

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

--5

f :: Integer -> Integer
f n | n <= 7 = n^2
    | otherwise = 2*n-1

g :: Integer -> Integer
g n | mod n 2 == 0 = div n 2
    | otherwise = 3*n+1

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (x, y, z) = (((f x) > (g x)) && ((f y) > (g y)) && ((f z) > (g z)))

---------------------------------------

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n >= 2 = fibonacci (n-1) + fibonacci (n-2)

------------------------

factorial :: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

------------------------

parteEntera :: Float -> Integer
parteEntera x | x >= 0 && x < 1 = 0
              | x >= 1 = 1 + parteEntera (x - 1)
              | x <= 0 && x > -1 = 0
              | x <= -1 = 1 + parteEntera (x + 1)

------------------------