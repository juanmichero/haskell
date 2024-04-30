-- 1.1

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 1.2

ultimo :: [t] -> t
ultimo (x:xs) | longitud (x:xs) == 1 = x
              | otherwise = ultimo xs

ultimoBis :: [t] -> t
ultimoBis [x] = x
ultimoBis (x:xs) = ultimoBis xs

-- 1.3

principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

-- 1.4

reverso :: [t] -> [t]
reverso [] = []
reverso xs = ultimo xs : reverso (principio xs) 

-- 2.1

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

pertenece2 ::  (Eq t) => t -> [t] -> Bool
pertenece2 _ [] = False
pertenece2 x (y:ys) = x == y || pertenece2 x ys

pertenece3 :: (Eq t) => t -> [t] -> Bool
pertenece3 _ [] = False
pertenece3 x y | x == head y = True
               | otherwise = pertenece x (tail y)

-- 2.2

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = x == head xs && todosIguales xs

-- 2.3

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True
todosDistintos (x:xs) | pertenece x xs = False
                      | otherwise = todosDistintos xs

-- 2.4

hayRepetidos ::  (Eq t) => [t] -> Bool
hayRepetidos [] = False
-- hayRepetidos [x] = False eventualmente llega al otro caso base, asi que no es necesario ponerlo
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

hayRepetidos2 ::  (Eq t) => [t] -> Bool
hayRepetidos2 [] = False
hayRepetidos2 (x:xs) = pertenece x xs || hayRepetidos xs

-- 2.5

quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = y : quitar x ys

-- 2.6 

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys) | x == y = quitarTodos x ys
                     | otherwise = y : quitarTodos x ys

-- 2.7

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
-- eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) | pertenece x xs = x : eliminarRepetidos (quitarTodos x xs)
                         | otherwise = x : eliminarRepetidos xs

-- 2.8
-- ??????????
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos x y = estaContenido x y && estaContenido y x

estaContenido :: (Eq t) => [t] -> [t] -> Bool
estaContenido [] ys = True
estaContenido (x:xs) ys = pertenece x ys && estaContenido xs ys

-- 2.9
-- ?????????
capicua :: (Eq t) => [t] -> Bool
capicua xs = xs == reverso xs

-- 3.1

sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- 3.2

productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- 3.3

maximo :: [Integer] -> Integer
maximo (x:[]) = x
maximo (x:(y:xs)) | x >= y = maximo (x:xs) -- (x:y:xs)
                  | otherwise = maximo (y:xs)

-- 3.4 

sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n (x:xs) = (x+n) : sumarN n xs

-- 3.5

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero xs = sumarN (head xs) xs

-- 3.6
 
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (ultimo xs) xs

-- 3.7

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | esPar x = x : pares xs
             | otherwise = pares xs

esPar :: Integer -> Bool
esPar n = mod n 2 == 0

-- 3.8

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

-- 3.9

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar xs = minimo xs : ordenar (quitar (minimo xs) xs)

minimo :: [Integer] -> Integer
minimo (x:[]) = x
minimo (x:y:xs) | x <= y = minimo (x:xs)
                | otherwise = minimo (y:xs)

-- 5.1

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [x] = [x]
sumaAcumulada (x:y:xs) = x : sumaAcumulada ((x+y):xs)

-- 5.2

-- aux
menorDivisor :: Integer -> Integer
menorDivisor 1 = 1
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n m | mod n m == 0 = m
                      | otherwise = menorDivisorDesde n (m+1)

esPrimo :: Integer -> Bool
esPrimo 1 = True
esPrimo n = menorDivisorDesde n 2 == n

factorizar :: Integer -> Integer -> [Integer]
factorizar 1 _ = []
factorizar n m | mod n m == 0 = m : factorizar (div n m) m
               | otherwise = factorizar n (m+1)

primos :: Integer -> [Integer]
primos n | esPrimo n = [n]
         | otherwise = factorizar n 2

descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [x] = primos x : []
descomponerEnPrimos (x:xs) = primos x : descomponerEnPrimos xs

-- 4.a

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) | x == ' ' && x == y = sacarBlancosRepetidos (y:xs)
                               | otherwise = x : sacarBlancosRepetidos (y:xs)

-- 4.b

contarPalabrasSinEspacios :: [Char] -> Integer
contarPalabrasSinEspacios xs = contarPalabras (sacarBlancosRepetidos (sacarPrimerBlanco xs))

contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras [x] = 1
contarPalabras (x:xs) | x == ' ' = 1 + contarPalabras xs
                      | otherwise = contarPalabras xs

sacarPrimerBlanco :: [Char] -> [Char]
sacarPrimerBlanco (x:xs) | x == ' ' = xs
                         | otherwise = (x:xs)

-- 4.c

-- palabras :: [Char] -> [[Char]]
-- palabras [] = [[]]
-- palabras [x] = [[x]]
-- palabras (x:xs) | 

palabra :: [Char] -> [[Char]]
palabra [x] = [[x]]
palabra xs = sacarBlancosRepetidos (sacarPrimerBlanco xs) : []