{- Paradigma Funcional - Clase 6
Temas:
- Evaluacion perezosa
- Listas infinitas
- Expresiones lambda
-}
module Library where
import PdePreludat

-- Evaluacion perezosa y listas infinitas
-- ======================================

-- Que sucede si en Haskell definimos la siguiente funcion?
-- Tener en cuenta conocimientos previos de otros lenguajes.

repetidos :: Number -> [Number]
repetidos x = x : repetidos x
muchosUnos = repetidos 1

-- Tipos de evaluacion
-- ====================

-- Evaluacion Ansiosa/estricta (Eager)
-- La utilizada en la mayoria de lenguajes (imperativos y orientados a objetos).

-- Evaluacion Perezosa/diferida (Lazy)
-- Mas comun en lenguajes funcionales

-- Ventajas:
-- * Permite crear estructuras infinitas (lleva a soluciones mas elegantes)
-- * Mayor performance en cierto tipos de operaciones (Al evitarnos calculos innecesarios)
-- Desventajas:
-- * Orden de evaluacion no deterministico
-- * Problemas con I/O y funciones que causen efecto 
-- Solucion: Monads (ver PDF abajo)
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf?from=https%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fmarktoberdorf%2Fmark.pdf
-- * Mayor consumo de RAM (Relacion con el garbage collector)

-- post sobre Haskell, evaluacion diferida y el uso de memoria:
-- https://making.pusher.com/making-efficient-use-of-memory-in-haskell/

-- post sobre el uso de memoria en Haskell y el algoritmo de garbage collection que utiliza:
-- https://making.pusher.com/latency-working-set-ghc-gc-pick-two/

-- correr ghci con solo 20mb disponibles: ghci +RTS -M20m
-- import Data.List
-- foldl' (+) 0 [1..10^6] -- foldl' es una version ansiosa de foldl

-- xs = [1..10^6] :: [Int]
-- foldl' (+) 0 xs

-- --> Ver posibles soluciones.
-- ++ Activar evaluacion estricta para todo el modulo
-- ++ Definir tipos de datos estrictos

-- --> Ver como otros lenguajes adoptan en parte la evaluacion perezosa.

-- Volvemos a las listas infinitas...
-- ==================================

naturales :: [Number]
naturales = [0..]

naturalesSinEl0 :: [Number]
naturalesSinEl0 = map (+1) naturales

pares :: [Number]
pares = filter even naturalesSinEl0

-- Hacer take 10 pares
-- Cuantos elementos fueron calculados de naturales? Cuantos de pares?

-- Operaciones
-- ===============
-- * Operaciones que terminan (acotando el infinito)
-- take, drop, head, (!!) 

-- * Operaciones que transforman la lista infinita, pero no disparan su evaluacion
-- map, filter

-- * Operaciones que no terminan (desplegando el infinito en la memoria de nuestra computadora)
-- elem, foldl, show, print, last, all, any, sum, product

-- Separacion: Definicion de la computacion VS evaluacion de la computacion
-- ejemplo: monads, free monads.

-- Concepto

-- Declaratividad
-- ==============
-- Me abstraigo de *como* se hacen las cosas y me concentro en *que* necesito.

-- Ejemplo: Calcular los primeros 10 numeros primos.

noEsDivisible x y = x `mod` y /= 0

esPrimo 1 = False
esPrimo 2 = True
esPrimo x = all (noEsDivisible x) [2..(x-1)]

primos = filter esPrimo naturalesSinEl0

-- finalmente obtengo los 10 primeros primos.
-- take 10 primos

-- Probar las siguientes funciones y ver que tarea realizan:
-- repeat :: a -> [a]
-- cycle :: [a] -> [a]

-- Expresiones lambda
-- ==================

-- Sintaxis:
-- \ parametros -> cuerpo de la funcion

-- ejemplo y uso: 
-- \x -> x + 2
-- (\x -> x + 2) 2 -- devuelve 4

-- Las expresiones lambdas permiten definir funciones anónimas que 
-- se usan en un contexto limitado (el de la misma función que estoy definiendo)

doble :: Number -> Number
doble numero = numero + numero

doble2 :: Number -> Number
doble2 = \x -> x + x

cuentaLoca :: Number -> Number -> Number -> Number
cuentaLoca = (\x y z -> 2 * x + y + z)

-- función anónima ad-hoc que permite sumar los elementos de una tupla:
sumaElementos :: [(Number, Number)] -> [Number]
sumaElementos tuplas = map (\(a,b) -> a + b ) tuplas

-- Las lambdas son útiles cuando queremos trabajar con 
-- funciones de orden superior y no tenemos necesidad de reutilizar 
-- una expresión en otro contexto

data Cliente = UnCliente {
    nombre :: String,
    edad :: Number
} deriving (Show)

beto = UnCliente "Beto" 25
leti = UnCliente "Leti" 45
alfonso = UnCliente "Alfonso" 45
maria = UnCliente "Maria" 30
xiomara = UnCliente "Xiomara" 50

clientes :: [Cliente]
clientes = [beto, leti, alfonso, maria, xiomara]

-- con composición y aplicación parcial:
mayoresDe30 :: [Cliente]
mayoresDe30 = filter ((>30). edad) clientes

mayoresDe30Delegando :: [Cliente]
mayoresDe30Delegando = filter esMayor clientes

esMayor :: Cliente -> Bool
esMayor cliente = (edad cliente) > 30

mayoresDe30ConLambda :: [Cliente]
mayoresDe30ConLambda = filter (\cliente -> edad cliente > 30) clientes

-- En ocasiones sucede que no podemos aplicar parcialmente una función 
-- ya que el valor que le queremos pasar no es el primero que espera sino otro, 

-- Por ejemplo si quiero saber si un nombre es exótico, 
-- que se cumple si tiene x, k, q o w, no sería correcto intentar hacer:
esExotico1 nombre = any (elem "XKQWxkqw") nombre  -- no Funciona

-- Ya que “xkqw” que es la lista en la cual quiero verificar si se encuentra 
-- uno de los caracteres del nombre, no es correcto tratar de aplicárselo a elem 
-- porque debería ser el segundo parámetro, no el primero

-- Posibles soluciones:
---- Usando la funcion flip:
esExotico2 :: String -> Bool
esExotico2 nombre = any (flip elem "XKQWxkqw") nombre

---- Usando una Expresion Lambda:
esExotico3 :: String -> Bool
esExotico3 nombre = any (\letra -> elem letra "XKQWxkqw") nombre

-- Usando las funciones ya definidas:
losMayoresDe30YExoticos1 :: [Cliente]
losMayoresDe30YExoticos1  = filter (\x -> (esExotico3 (nombre x) && esMayor x)) clientes

-- Sin delegar (no recomendado):
losMayoresDe30YExoticos2 :: [Cliente]
losMayoresDe30YExoticos2 = filter (\cliente -> (any (\letra -> elem letra "XKQWxkqw") (nombre cliente)) && (edad cliente) > 30) clientes
