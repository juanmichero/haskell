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


-- (x,y) (a,b) x=a && y=b || x=b && y=a

-- relacionesValidas :: [(Int, Int)] -> Bool
-- relacionesValidas 

tuplasRepetidas :: [(Int, Int)] -> Bool
tuplasRepetidas [] = False
tuplasRepetidas [a, b] = a == b || fst a == snd b && snd a == fst b 

componentesIguales :: (Int, Int) -> Bool
componentesIguales (x,y) = x == y