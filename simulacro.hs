-- Template de funciones a implementar
-- relacionesValidas :: [(String, String)] -> Bool
-- relacionesValidas [] = False

-- personas :: [(String, String)] -> [String]
-- personas [] = "nadie"

-- amigosDe String -> [(String, String)] -> [String]
-- amigosDe "nadie" [] = "nadie"

-- personaConMasAmigos :: [(String, String)] -> String
-- personaConMasAmigos [] = "yo" 


--------------------------------


-- Ejemplo de HUnit

-- import Test.HUnit
-- import Simulacro

-- main = runTestTT tests

-- tests = test [
-- -- "nombre" ~: (funcion parametros) ~?= resultado_esperado
-- "componentes repetidas" ~: (relacionesValidas [("ana", "ana")]) ~?= False,
-- "tupla repetida" ~: (relacionesValidas [("ana", "pedro"), ("ana", "pedro")]) ~?= False,
-- "tupla repetida invertida" ~: (relacionesValidas [("ana", "pedro"), ("pedro", "ana")]) ~?= False,
-- "todas diferentes" ~: (relacionesValidas [("ana", "pedro"), ("ana", "carlos")]) ~?= True
-- ]

-- -- Ejemplos

-- usuario1 = "Juan"
-- usuario2 = "Natalia"
-- usuario3 = "Pedro"

-- relacion1_2 = (usuario1, usuario2)
-- relacion1_1 = (usuario1, usuario1)
-- relacion1_3 = (usuario1, usuario3)


-- -- FUNCIONES PARA TESTING, NO BORRAR
-- -- expectAny permite saber si el elemenot que devuelve la función es alguno de los esperados
-- expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- -- sonIguales permite ver que dos listas sean iguales si no importa el orden
-- quitar :: (Eq t) => t -> [t] -> [t]
-- -- requiere x pertenece a y
-- quitar x (y:ys)
-- | x == y = ys
-- | otherwise = y : quitar x ys

-- incluido :: (Eq t) => [t] -> [t] -> Bool
-- incluido [] l = True
-- incluido (x:c) l = elem x l && incluido c (quitar x l)

-- sonIguales :: (Eq t) => [t] -> [t] -> Bool
-- sonIguales xs ys = incluido xs ys && incluido ys xs 


--------------------------------------


-- Ejercicio 1
-- Para empezar a diseñar la novedosa y rupturista red social Y el famoso Elio Mark nos ha pedido que desarrollemos algunas funciones básicas, que tendrán como objetivo representar algunas relaciones e interacciones entre los usuarios. Para esto nos envió las siguientes especificaciones en lenguaje semiformal y nos pidió que hagamos el desarrollo enteramente en Haskell, utilizando los tipos requeridos y solamente las funciones que se ven en Introducción a la Programación de Exactas-UBA.

-- problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
--   requiere: {True}
--   asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}
-- }
-- 1 A los fines de este problema consideraremos que dos tuplas son iguales si el par de elementos que las componen (sin importar el orden) son iguales.

-- problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--   requiere: {relacionesValidas(relaciones)}
--   asegura: {res no tiene elementos repetidos}
--   asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
-- }

-- problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
--   requiere: {relacionesValidas(relaciones)}
--   asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
-- }

-- problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
--   requiere: {relaciones no vacía}
--   requiere: {relacionesValidas(relaciones)}
--   asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}
-- }


--------------------------------

-- 1

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs) = not (tuplasIguales x) && not (tuplasRepetidas x xs) && relacionesValidas xs

tuplasIguales :: (String,String) -> Bool
tuplasIguales (a,b) = a == b

tuplasRepetidas :: (String, String) -> [(String, String)] -> Bool
tuplasRepetidas _ [] = False
tuplasRepetidas (a,b) ((x1,x2):xs) = (a == x1 && b == x2) || (a == x2 && b == x1) || tuplasRepetidas (a,b) xs

-- 2 [("carlos","juan"),("sancho","panza"),("john","sancho")] => ["carlos","juan","sancho","panza","john"]

personas :: [(String, String)] -> [String]
personas [] = []
personas (x:xs) = eliminarRepetidos (fst x : snd x : personas xs)

-- aux

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (y:ys) | x == y = quitarTodos x ys
                     | otherwise = y : quitarTodos x ys

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = x : eliminarRepetidos (quitarTodos x xs)
                         | otherwise = x : eliminarRepetidos xs

-- 3 fst x == persona || snd x == persona = (x,persona) => [x,persona]

amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe x (y:ys) | x == (fst y) = (snd y) : amigosDe x ys
                  | x == (snd y) = (fst y) : amigosDe x ys
                  | otherwise = amigosDe x ys

-- 4

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [x] = ""
personaConMasAmigos (x:y:xs) | contarPersona (fst x) (x:y:xs) >= contarPersona (fst y) (x:y:xs) = fst x
                             | contarPersona (snd x) (x:y:xs) >= contarPersona (snd y) (x:y:xs) = snd x
                             | otherwise = personaConMasAmigos xs

contarPersona :: String -> [(String, String)] -> Int
contarPersona _ [] = 0
contarPersona x (y:ys) | x == (fst y) || x == (snd y) = 1 + contarPersona x ys
                       | otherwise = contarPersona x ys

maximo :: [Int] -> Int
maximo (x:[]) = x
maximo (x:y:xs) | x >= y = maximo (x:xs) 
                  | otherwise = maximo (y:xs)