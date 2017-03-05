-- Octavio, Gianatiempo - 280/10 - ogianatiempo@gmail.com
-- Jorge, Mamani Fernandez - 23/08 - papelyto_god@hotmail.com

-- Importamos la Parte1 -------------------------------------------------------
import Parte1

-- EJERCICIO 8 ----------------------------------------------------------------

-- Dado un caracter, devuelve el caracter siguiente.
-- Es funcion auxiliar de cifrarCaracter
siguiente :: Char -> Char
siguiente 'Z' = 'A'
siguiente ' ' = ' '
siguiente c = succ c

-- Dado un caracter y un Desplazamiento, devuelve el caracter que resulta de la
-- aplicacion de dicho desplazamiento.
-- Es función auxiliar de cifrarTexto
cifrarCaracter :: Char -> Desplazamiento -> Char
cifrarCaracter c 0 = c
cifrarCaracter c 1 = siguiente c
cifrarCaracter c n | n > 0 = siguiente (cifrarCaracter c ((mod n 26)-1))
                   | otherwise = cifrarCaracter c (n+26)

-- Dado un texto y un Desplazamiento, desplaza cada caracter del texto de forma
-- recursiva según el Desplazamiento indicado.
-- Es función auxiliar de cifrarCesar
cifrarTexto :: Texto -> Desplazamiento -> Texto
cifrarTexto [] n = []
cifrarTexto (c:cs) n = cifrarCaracter c n : cifrarTexto cs n

cifrarCesar :: Mensaje -> Desplazamiento -> Mensaje
cifrarCesar (TextoClaro t) n = CifradoCesar (TextoClaro (cifrarTexto t n)) n
cifrarCesar (CifradoCesar m n1) n2 = CifradoCesar (cifrarCesar m n2) n1
cifrarCesar (CifradoReverso m) n = CifradoReverso (cifrarCesar m n)
cifrarCesar (CifradoPalabrasReverso m) n = CifradoPalabrasReverso (cifrarCesar m n)

-- EJERCICIO 9 ----------------------------------------------------------------

-- Dada un Texto devuelve True en las posiciones que hay espacios.
-- Es funcion auxiliar de posicionesEspacios
esEspacio :: Texto -> [Bool]
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
-- corresponden a posiciones que son True (las listas deben ser del mismo largo).
-- Es funcion auxiliar de posicionesEspacios
subsetVerdadero :: [Bool] -> [a] -> [a]
subsetVerdadero [] [] = []
subsetVerdadero (True:bs) (e:es) = e : subsetVerdadero bs es
subsetVerdadero (False:bs) (e:es) = subsetVerdadero bs es

-- Dada un Texto devuelve las posiciones de los espacios.
-- El primer caracter es el caracter 0.
-- Es funcion auxiliar de reversoPalabras
posicionesEspacios :: Texto -> [Integer]
posicionesEspacios s = subsetVerdadero (esEspacio s) (posiciones s 0)

-- Dado un Texto y un Integer, recorta la parte izquierda del texto para que
-- comience justo después de la posicion indicada por el integer. Es decir se
-- queda con la parte derecha. (la primera posicion es 0).
-- Es funcion auxiliar de listaPalabras
derecha :: Texto -> Integer -> Texto
derecha (c:cs) 0 = cs
derecha (c:cs) n = derecha cs (n-1)

-- Dado un texto y un integer, recorta la parte derecha del texto justo antes
-- de la posicion indicada por el integer. Es decir, se queda con la parte
-- izquierda. (la primera posicion es 0).
-- Es funcion auxiliar de listaPalabras
izquierda :: Texto -> Integer -> Texto
izquierda s 0 = []
izquierda (c:cs) n = c : izquierda cs (n-1)

-- Dado un Texto, una lista de integers representando los espacios y un Integer
-- que representa el espacio anterior (debe ser 0 al inicio), separa
-- las palabras del Texto y las pone en una lista.
-- Es funcion auxiliar de reversoPalabras
-- OJO QUE SI HUBIERA ESPACIOS AL PRINCIPIO O AL FINAL LOS TOMA COMO PALABRAS
-- VACIAS, PERO ESTO ES BUENO PARA QUE DESPUES listaPalabrasATexto AGREGUE
-- DICHOS ESPACIOS. ES MEDIO DESPROLIJO PERO ANDA, PENSAR MEJOR FORMA
listaPalabras :: Texto -> [Integer] -> Integer -> [Texto]
-- Casos base
listaPalabras s [] 0 = [s] -- Esto seria una sola palabra sin espacios
listaPalabras s [] e = derecha s e : [] -- Esto seria la última palabra de un texto
-- Casos que inician con espacio
listaPalabras s (0:es) 0 = izquierda s 0 : listaPalabras s es 0
listaPalabras (' ':s) (e:es) n = derecha (izquierda (' ':s) e) n : listaPalabras (' ':s) es e
-- Casos que no inician con espacio
listaPalabras s (e:es) 0 = izquierda s e : listaPalabras s es e
listaPalabras s (e:es) n = derecha (izquierda s e) n : listaPalabras s es e

-- Dada una lista de palabras, revierte cada una de ellas.
-- Es funcion auxiliar de reversoPalabras
reversoListaPalabras :: [Texto] -> [Texto]
reversoListaPalabras [] = []
reversoListaPalabras (p:ps) = reverso p : reversoListaPalabras ps

-- Dada una lista de palabras genera un texto con las palabras separadas por
-- espacios.
-- Es funcion auxiliar de reversoPalabras
listaPalabrasATexto :: [Texto] -> Texto
listaPalabrasATexto (p:[]) = p
listaPalabrasATexto (p:ps) = p ++ " " ++ listaPalabrasATexto ps

-- Dado un texto, devuelve cada palabra del texto al revés.
-- Es funcion auxiliar de cifrarPalabrasReverso
reversoPalabras :: Texto -> Texto
reversoPalabras s = listaPalabrasATexto (reversoListaPalabras (listaPalabras s (posicionesEspacios s) 0))

cifrarPalabrasReverso :: Mensaje -> Mensaje
cifrarPalabrasReverso (TextoClaro t) = CifradoPalabrasReverso (TextoClaro (reversoPalabras t))
cifrarPalabrasReverso (CifradoCesar m n1) = CifradoCesar (cifrarPalabrasReverso m) n1
cifrarPalabrasReverso (CifradoReverso m) = CifradoReverso (cifrarPalabrasReverso m)
cifrarPalabrasReverso (CifradoPalabrasReverso m)= CifradoPalabrasReverso (cifrarPalabrasReverso m)
