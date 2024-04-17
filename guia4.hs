--guia 4

factorial :: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)

--1

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n >= 2 = fibonacci (n-1) + fibonacci (n-2)

--2

parteEntera :: Float -> Integer
parteEntera x | x >= 0 && x < 1 = 0
              | x >= 1 = 1 + parteEntera (x - 1)
              | x <= 0 && x > -1 = 0
              | x <= -1 = 1 + parteEntera (x + 1)

-- | x < 1 && x >= 0 = 0
-- | x >= 1 = 1 + parteEntera (x - 1)
-- | otherwise = (-1) + parteEntera (x + 1)

--3

esDivisible :: Integer -> Integer -> Bool
-- esDivisible _ 0 = error ""
esDivisible x y | x < y = False
                | x == y = True
                | otherwise = esDivisible (x - y) y

--4

sumaImpares :: Integer -> Integer
sumaImpares 0 = 0
sumaImpares 1 = 1
sumaImpares n | n > 1 = (2*n-1) + sumaImpares (n-1)

--5

medioFact :: Integer -> Integer
medioFact n | n == 0 = 1
            | n == 1 = 1
            | n > 1 = n * medioFact (n - 2)

--6

sumaDigitos :: Integer -> Integer
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

--7

-- todosDigitosIguales :: Integer -> Bool
-- todosDigitosIguales n | mod n 10 == n = True
                      

--13

sumatoriaDesdeM :: Int -> Int -> Int
sumatoriaDesdeM i 0 = 0
-- sumatoriaDesdeM i 1 = i es una obviedad
sumatoriaDesdeM i m = i^m + sumatoriaDesdeM i (m-1)

sumatoriaDoble :: Int -> Int -> Int
sumatoriaDoble 0 _ = 0
sumatoriaDoble n m = sumatoriaDesdeM n m + sumatoriaDoble (n-1) m