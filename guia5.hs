--2.1

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

pertenece2 :: Int -> [Int] -> Bool
pertenece2 _ [] = False
pertenece2 x (y:ys) = x == y || pertenece2 x ys

pertenece3 :: Int -> [Int] -> Bool
pertenece3 _ [] = False
pertenece3 x y | x == head y = True
               | otherwise = pertenece x (tail y)

--2.2

todosIguales :: [Int] -> Bool
todosIguales [] = False
todosIguales (x:xs) = x == head xs || todosIguales xs

--2.3



--2.4

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
-- hayRepetidos [x] = False eventualmente llega al otro caso base, asi que no es necesario ponerlo
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

hayRepetidos2 :: [Int] -> Bool
hayRepetidos2 [] = False
hayRepetidos2 (x:xs) = pertenece x xs || hayRepetidos xs

--2.5

quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar x (y:ys) | x == y = ys
                | otherwise = y : quitar x ys

--3.3

maximo :: [Integer] -> Integer
maximo (x:[]) = x
maximo (x:(y:xs)) | x >= y = maximo (x:xs)
                  | otherwise = maximo (y:xs)