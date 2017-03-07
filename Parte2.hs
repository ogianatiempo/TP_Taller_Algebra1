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

-- Ejemplo función cifrarCesar
--
-- CifradoCesar (TextoClaro "IPMB UJP MVDBT") 1
-- it :: Mensaje
--
-- *Main> cifrarCesar (TextoClaro "HOLA TIO LUCAS") (-5)
-- CifradoCesar (TextoClaro "CJGV ODJ GPXVN") (-5)
-- it :: Mensaje
--
-- *Main> cifrarCesar (CifradoReverso (CifradoPalabrasReverso (TextoClaro "LUCAS TIO HOLA"))) 1
-- CifradoReverso (CifradoPalabrasReverso (CifradoCesar (TextoClaro "MVDBT UJP IPMB") 1))
-- it :: Mensaje



-- EJERCICIO 9 ----------------------------------------------------------------

-- Dado un Texto y un Integer que representa el valor para la posicion del
-- primer caracter del texto, devuelve las posiciones de los espacios.
-- Es funcion auxiliar de reversoPalabras
posicionesEspacios :: Texto -> Integer -> [Integer]
posicionesEspacios [] n = []
posicionesEspacios (' ':cs) n = n : posicionesEspacios cs (n+1)
posicionesEspacios (c:cs) n = posicionesEspacios cs (n+1)

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
reversoPalabras s = listaPalabrasATexto (reversoListaPalabras (listaPalabras s (posicionesEspacios s 0) 0))

cifrarPalabrasReverso :: Mensaje -> Mensaje
cifrarPalabrasReverso (TextoClaro t) = CifradoPalabrasReverso (TextoClaro (reversoPalabras t))
cifrarPalabrasReverso (CifradoCesar m n1) = CifradoCesar (cifrarPalabrasReverso m) n1
cifrarPalabrasReverso (CifradoReverso m) = CifradoReverso (cifrarPalabrasReverso m)
cifrarPalabrasReverso (CifradoPalabrasReverso m)= CifradoPalabrasReverso (cifrarPalabrasReverso m)

-- EJERCICIO 10 ------------------------------------------------------------------
-- Si definimos acá descifrar usando funciones de la parte2, si cargamos sólo
-- la parte1 andará? Probar y sinó consultar.

descifrar :: Mensaje -> Texto
descifrar (TextoClaro t) = t
descifrar (CifradoReverso m) = reverso (descifrar m)
descifrar (CifradoCesar m n) = cifrarTexto (descifrar m) (-n)
descifrar (CifradoPalabrasReverso m) = reversoPalabras (descifrar m)

-- Ejemplo función descifrar
--
-- *Main> descifrar (CifradoReverso (TextoClaro "ERTSOP LE"))
-- "EL POSTRE"
-- it :: Texto
-- 
-- *Main> descifrar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- "HOLA TAROLAS"
-- it :: Texto
