-- Octavio, Gianatiempo - 280/10 - ogianatiempo@gmail.com
-- Jorge, Mamani Fernandez - 23/08 - papelyto_god@hotmail.com

{-
Ver si aplica para el TP pero no se sugiere usar:

clase Enum para tipos enumerados y funciones succ y pred
En el caso del tipo enumerado para los días:
next :: Dia -> Dia
next Sabado = Domingo
next d = succ d
-}

-- Importamos la Parte1 -------------------------------------------------------
import Parte1

-- Ejercicio 8 ----------------------------------------------------------------
siguiente :: Char -> Char
siguiente 'Z' = 'A'
siguiente ' ' = ' '
siguiente c = succ c

-- Por ahora funciona solo para Desplazamientos positivos, pensar que pasa con
-- Desplazamientos negativos
cifrarCaracter :: Char -> Desplazamiento -> Char
cifrarCaracter c 0 = c
cifrarCaracter c 1 = siguiente c
cifrarCaracter c n = siguiente (cifrarCaracter c ((mod n 27)-1))

cifrarTexto :: Texto -> Desplazamiento -> Texto
cifrarTexto [] n = []
cifrarTexto (c:cs) n = cifrarCaracter c n : cifrarTexto cs n

cifrarCesar :: Mensaje -> Desplazamiento -> Mensaje
cifrarCesar (TextoClaro t) n = CifradoCesar (TextoClaro (cifrarTexto t n)) n
cifrarCesar (CifradoCesar m n1) n2 = CifradoCesar (cifrarCesar m n2) n1
cifrarCesar (CifradoReverso m) n = CifradoReverso (cifrarCesar m n)
cifrarCesar (CifradoPalabrasReverso m) n = CifradoPalabrasReverso (cifrarCesar m n)

-- Ejercicio 9 ----------------------------------------------------------------

-- Dada lista de caracteres devuelve True en las posiciones que hay espacios
esEspacio :: String -> [Bool]
esEspacio [] = []
esEspacio (' ':cs) = True : esEspacio cs
esEspacio (_:cs) = False : esEspacio cs

-- Dada una lista y un nro inicial, asigna nros a las posiciones contando a
-- partir del nro inicial
posiciones :: [a] -> Integer -> [Integer]
posiciones [] n = []
posiciones (c:cs) n = n : posiciones cs (n+1)

-- Dada una lista de Bools, selecciona de una lista los elementos que
-- corresponden a posiciones que son True (las listas deben ser del mismo largo)
subsetVerdadero :: [Bool] -> [a] -> [a]
subsetVerdadero [] [] = []
subsetVerdadero (True:bs) (e:es) = e : subsetVerdadero bs es
subsetVerdadero (False:bs) (e:es) = subsetVerdadero bs es

-- Dada una lista de caracteres devuelve las posiciones de los espacios
-- El primer caracter es el caracter 0
posicionesEspacios :: String -> [Integer]
posicionesEspacios s = subsetVerdadero (esEspacio s) (posiciones s 0)

-- Dado un string y un integer, recorta la parte izquierda del string para que
-- comience en la posicion indicada por el integer (la primera posicion es 0)
stringAPartirDe :: String -> Integer -> String
stringAPartirDe s 0 = s
stringAPartirDe (c:cs) n = stringAPartirDe cs (n-1)

-- Dado un string y un integer, recorta la parte derecha del string justo antes
-- de la posicion indicada por el integer (la primera posicion es 0)
stringHasta :: String -> Integer -> String
stringHasta s 0 = []
stringHasta (c:cs) n = c : stringHasta cs (n-1)
