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