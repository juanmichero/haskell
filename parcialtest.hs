-- 2c 2023

-- 2

formulasValidas :: [(String, String)] -> Bool
formulasValidas [] = True
formulasValidas [(p,v)] = p /= v
formulasValidas ((p,v):xs) = p /= v && noAparecen (p,v) xs && formulasValidas xs

noAparecen :: (String, String) -> [(String, String)] -> Bool
noAparecen _ [] = True
noAparecen (p,v) ((p1,v1):xs) = p /= p1 && p /= v1 && v /= p1 && v /= v1 && noAparecen (p,v) xs -- mismo con fst y snd

-- 1

votosEnBlanco :: [(String, String)] -> [Int] -> Int -> Int
votosEnBlanco _ votos cantTotalVotos = cantTotalVotos - sumaVotos votos
votosEnBlanco _ [] cantTotalVotos = cantTotalVotos
votosEnBlanco formulas (x:xs) cantTotalVotos = (votosEnBlanco formulas xs cantTotalVotos) - x

sumaVotos :: [Int] -> Int
sumaVotos [] = 0
sumaVotos (x:xs) = x + sumaVotos xs

-- 3

-- porcentajeDeVotos :: String -> [(String, String)] -> [Int] -> Float
-- porcentajeDeVotos p formulas votosValidos = (cantidadVotosCandidato p formulas votosValidos) division (sumaVotos votosValidos) * 100

-- cantidadVotosCandidato :: String -> [(String, String)] -> [Int] -> Int
-- cantidadVotosCandidato p ((p1,v1):xs) (y:ys) | p == p1 = y
--                                              | otherwise = cantidadVotosCandidato p xs ys
-- cantidadVotosCandidato p formulas votos = i_esimo (damePosicion p formulas) votos

-- damePosicion :: String -> [(String, String)] -> Int
-- damePosicion p ((p1,v1):xs) | p == p1 = 0
--                             | otherwise = 1 + damePosicion p xs

-- 4

maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) | x >= maximo xs = x
              | otherwise = maximo xs
-- maximo (x:y:xs) | x >= y maximo (x:xs)
--                 | otherwise = maximo (y:xs)

proximoPresidente :: [(String, String)] -> [Int] -> String
proximoPresidente [(p,v)] [x] = p 
proximoPresidente ((p,v):ys) (x:xs) | maximo (x:xs) == x = p
                                    | otherwise = proximoPresidente ys xs

-------------------

-- simulacro

-- personasConRepetidos :: [(String, String)] -> [String]
-- personasConRepetidos [(p1,p2)] == (p1:p2:[]) = [p1,p2]
-- personasConRepetidos (p1,p2):xs = p1:p2:(personasConMasRep xs)
