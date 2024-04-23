--guia 4

factorial :: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

-- 1

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n >= 2 = fibonacci (n-1) + fibonacci (n-2)

-- 2

parteEntera :: Float -> Integer
parteEntera x | x >= 0 && x < 1 = 0
              | x >= 1 = 1 + parteEntera (x - 1)
              | x <= 0 && x > -1 = 0
              | x <= -1 = 1 + parteEntera (x + 1)

-- | x < 1 && x >= 0 = 0
-- | x >= 1 = 1 + parteEntera (x - 1)
-- | otherwise = (-1) + parteEntera (x + 1)

-- 3

esDivisible :: Integer -> Integer -> Bool
-- esDivisible _ 0 = error ""
esDivisible x y | x < y = False
                | x == y = True
                | otherwise = esDivisible (x - y) y

-- 4

sumaImpares :: Integer -> Integer
sumaImpares 0 = 0
sumaImpares 1 = 1
sumaImpares n | n > 1 = (2 * n - 1) + sumaImpares (n - 1)

-- 5

medioFact :: Integer -> Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n > 1 = n * medioFact (n - 2)

-- 6

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

-- 7

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | otherwise = (mod n 10) == (mod (div n 10) 10) && todosDigitosIguales(div n 10)

-- 8

cantDigitos :: Integer -> Integer 
cantDigitos x | x < 10 = 1
              | otherwise = 1 + cantDigitos (div x 10)

-- n = número natural con x cantidad de digitos
-- i = número natural menor o igual a la cantidad de digitos de n

iesimoDigito :: Integer -> Integer -> Integer -- devuelve el i-esimo digito de n (digito de n en la posición 'i')
iesimoDigito n i | cantDigitos n == 1 = n
                 | i < n && i == cantDigitos n = mod n 10
                 | otherwise = iesimoDigito (div n 10) i

-- iesimoDigito :: Int -> Int -> Int
-- iesimoDigito n i = mod (div n (10 ^ ((cantDigitos n) - i))) 10

-- 10

f1 :: Int -> Int
f1 x | x == 0 = 1
     | x > 0 = 2^x + f1 (x-1)

f2 :: Int -> Int -> Int
f2 x y | x == 1 = y
       | otherwise = y^x + f2 (x-1) y

f3 :: Int -> Int -> Int
f3 x y = f2 (2*x) y

-- ???????
-- f4 :: Int -> Int -> Int
-- f4 x y = f2 (2*x) y - f2 (x-1) y 

-- 11

eAprox :: Int -> Float
eAprox n | n == 0 = 1
         | otherwise = 1 / fromIntegral (factorial n) + eAprox (n - 1)

e :: Float 
e = eAprox 10

-- 12

-- raizDe2Aprox :: Integer -> Float
-- raizDe2Aprox n | n == 1 = 1
--                | n > 1 = 2 + 1 / (raizDe2Aprox (n - 1))

-- 13

sumatoriaDesdeM :: Int -> Int -> Int
sumatoriaDesdeM i 0 = 0
-- sumatoriaDesdeM i 1 = i es una obviedad
sumatoriaDesdeM i m = i^m + sumatoriaDesdeM i (m-1)

sumatoriaDoble :: Int -> Int -> Int
sumatoriaDoble 0 _ = 0
sumatoriaDoble n m = sumatoriaDesdeM n m + sumatoriaDoble (n-1) m

-- 14
-- corroborar xdxd
sumaPotenciasN :: Integer -> Integer -> Integer -> Integer
sumaPotenciasN q 1 b = q^(1+b)
sumaPotenciasN q n b = q^(n+b) + sumaPotenciasN q (n-1) b

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n 1 = q^(n+1)
sumaPotencias q n m = sumaPotenciasN q n m + sumaPotencias q n (m-1)

-- 15