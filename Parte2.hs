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

-- Dada lista de caracteres devuelve True en las posiciones que hay espacios.
-- Es funcion auxiliar de posicionesEspacios
esEspacio :: String -> [Bool]
esEspacio [] = []
esEspacio (' ':cs) = True : esEspacio cs
esEspacio (_:cs) = False : esEspacio cs

-- Dada una lista y un nro inicial, asigna nros a las posiciones contando a
-- partir del nro inicial.
-- Es funcion auxiliar de posicionesEspacios
posiciones :: [a] -> Integer -> [Integer]
posiciones [] n = []
posiciones (c:cs) n = n : posiciones cs (n+1)

-- Dada una lista de Bools, selecciona de una lista los elementos que
-- corresponden a posiciones que son True (las listas deben ser del mismo largo)
-- Es funcion auxiliar de posicionesEspacios
subsetVerdadero :: [Bool] -> [a] -> [a]
subsetVerdadero [] [] = []
subsetVerdadero (True:bs) (e:es) = e : subsetVerdadero bs es
subsetVerdadero (False:bs) (e:es) = subsetVerdadero bs es

-- Dada una lista de caracteres devuelve las posiciones de los espacios
-- El primer caracter es el caracter 0
-- Es funcion auxiliar de reversoPalabras
posicionesEspacios :: String -> [Integer]
posicionesEspacios s = subsetVerdadero (esEspacio s) (posiciones s 0)

-- Dado un string y un integer, recorta la parte izquierda del string para que
-- comience justo después de la posicion indicada por el integer. Es decir se
-- queda con la parte derecha. (la primera posicion es 0)
-- Es funcion auxiliar de listaPalabras
derecha :: String -> Integer -> String
derecha (c:cs) 0 = cs
derecha (c:cs) n = derecha cs (n-1)

-- Dado un string y un integer, recorta la parte derecha del string justo antes
-- de la posicion indicada por el integer. Es decir, se queda con la parte
-- izquierda. (la primera posicion es 0)
-- Es funcion auxiliar de listaPalabras
izquierda :: String -> Integer -> String
izquierda s 0 = []
izquierda (c:cs) n = c : izquierda cs (n-1)

-- Dado un string, una lista de integers representando los espacios y un Integer
-- que representa el espacio anterior (debe ser 0 al inicio), separa
-- las palabras del string y las pone en una lista.
-- Es funcion auxiliar de reversoPalabras
-- OJO QUE SI HUBIERA ESPACIOS AL PRINCIPIO O AL FINAL LOS TOMA COMO PALABRAS
-- VACIAS, PERO ESTO ES BUENO PARA QUE DESPUES listaPalabrasAString AGREGUE
-- DICHOS ESPACIOS. ES MEDIO DESPROLIJO PERO ANDA, PENSAR MEJOR FORMA
listaPalabras :: String -> [Integer] -> Integer -> [String]
listaPalabras s [] 0 = [s] -- Esto seria una sola palabra sin espacios
listaPalabras s [] e = derecha s e : []
listaPalabras s (0:es) 0 = izquierda s 0 : listaPalabras s es 0 -- Esto seria un String que empieza con espacio y estamos en la primer palabra
listaPalabras (' ':s) (e:es) n = derecha (izquierda (' ':s) e) n : listaPalabras (' ':s) es e -- Esto seria un String que empieza con espacio y ya no estamos en la primer palabra
listaPalabras s (e:es) 0 = izquierda s e : listaPalabras s es e
listaPalabras s (e:es) n = derecha (izquierda s e) n : listaPalabras s es e

-- Dada una lista de palabras, revierte cada una de ellas
-- Es funcion auxiliar de reversoPalabras
reversoListaPalabras :: [String] -> [String]
reversoListaPalabras [] = []
reversoListaPalabras (p:ps) = reverso p : reversoListaPalabras ps

-- Dada una lista de palabras genera un string con las palabras separadas por
-- espacios
-- Es funcion auxiliar de reversoPalabras
listaPalabrasAString :: [String] -> String
listaPalabrasAString (p:[]) = p
listaPalabrasAString (p:ps) = p ++ " " ++ listaPalabrasAString ps

-- Esta es la funcion auxiliar de cifrarPalabrasReverso
reversoPalabras :: String -> String
reversoPalabras s = listaPalabrasAString (reversoListaPalabras (listaPalabras s (posicionesEspacios s) 0))

-- cifrarPalabrasReverso
cifrarPalabrasReverso :: Mensaje -> Mensaje
cifrarPalabrasReverso (TextoClaro t) = CifradoPalabrasReverso (TextoClaro (reversoPalabras t))
cifrarPalabrasReverso (CifradoCesar m n1) = CifradoCesar (cifrarPalabrasReverso m) n1
cifrarPalabrasReverso (CifradoReverso m) = CifradoReverso (cifrarPalabrasReverso m)
cifrarPalabrasReverso (CifradoPalabrasReverso m)= CifradoPalabrasReverso (cifrarPalabrasReverso m)
