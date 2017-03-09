-- Octavio, Gianatiempo - 280/10 - ogianatiempo@gmail.com
-- Jorge, Mamani Fernandez - 23/08 - papelyto_god@hotmail.com



-- Definicion de los tipos -----------------------------------------------------
-- Renombre para tipo Texto
type Texto = [Char]

-- Renombre para tipo Desplazamiento
type Desplazamiento = Integer

-- Definición del tipo recursivo Mensaje extendido (EJERCICIO 7)
data Mensaje = TextoClaro Texto
               | CifradoReverso Mensaje
               | CifradoCesar Mensaje Desplazamiento
               | CifradoPalabrasReverso Mensaje
               deriving (Eq, Show)



-- Funciones de la parte1 extendidas (EJERCICIO 10) ----------------------------
-- EJERCICIO 1 -----------------------------------------------------------------
pertenece :: Char -> String -> Bool
pertenece c [] = False
pertenece c (l:ls) = c == l || pertenece c ls

esCaracterPermitido :: Char -> Bool
esCaracterPermitido c = pertenece c (' ':['A'..'Z'])

esTextoPermitido :: Texto -> Bool
esTextoPermitido [] = True
esTextoPermitido (l:ls) = esCaracterPermitido l && esTextoPermitido ls

crearMensaje :: Texto -> Mensaje
crearMensaje t | esTextoPermitido t && length t > 0 = TextoClaro t

-- Ejemplos Función crearMensaje
--
-- *Main> crearMensaje "HOLA QUE TAL"
-- TextoClaro "HOLA QUE TAL"
-- it :: Mensaje
--
-- *Main> crearMensaje "Hola que tal"
-- *** Exception: Parte1.hs:72:1-50: Non-exhaustive patterns in function
-- crearMensaje

-- EJERCICIO 2 -----------------------------------------------------------------
esMensajeCifrado :: Mensaje -> Bool
esMensajeCifrado (TextoClaro t) = False
esMensajeCifrado _ = True

-- *Main> esMensajeCifrado (TextoClaro "HOLA TAROLAS")
-- False
-- it :: Bool
--
-- *Main> esMensajeCifrado (CifradoReverso (TextoClaro "SALORAT ALOH"))
-- True
-- it :: Bool
--
-- *Main> esMensajeCifrado (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- True
-- it :: Bool

-- EJERCICIO 3 -----------------------------------------------------------------
reverso :: Texto -> Texto
reverso [] = []
reverso (t:ts) = reverso ts ++ [t]

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro t) = CifradoReverso (TextoClaro (reverso t))
cifrarReverso (CifradoReverso m) = CifradoReverso (cifrarReverso m)
cifrarReverso (CifradoCesar m n) = CifradoCesar (cifrarReverso m) n
cifrarReverso (CifradoPalabrasReverso m) = CifradoPalabrasReverso (cifrarReverso m)

-- Ejemplo función cifrarReverso
--
-- *Main> cifrarReverso (TextoClaro "EL POSTRE")
-- CifradoReverso (TextoClaro "ERTSOP LE")
--
-- *Main> cifrarReverso (cifrarReverso (TextoClaro "EL POSTRE"))
-- CifradoReverso (CifradoReverso (TextoClaro "EL POSTRE"))
--
-- *Main> cifrarReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- CifradoReverso (CifradoCesar (CifradoPalabrasReverso (CifradoReverso (TextoClaro "CNQJ UCNQTCV"))) 2)
-- it :: Mensaje

-- EJERCICIO 4 -----------------------------------------------------------------
extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro t) = t
extraerMensajeParaEnvio (CifradoReverso m) = extraerMensajeParaEnvio m
extraerMensajeParaEnvio (CifradoCesar m n) = extraerMensajeParaEnvio m
extraerMensajeParaEnvio (CifradoPalabrasReverso m) = extraerMensajeParaEnvio m

-- Ejemplo Función extraerMensajeParaEnvio
--
-- *Main> extraerMensajeParaEnvio (CifradoReverso (TextoClaro "ERTSOP LE"))
-- "ERTSOP LE"
-- it :: Texto
--
-- *Main> extraerMensajeParaEnvio (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- "VCTQNCU JQNC"
-- it :: Texto

-- EJERCICIO 5 -----------------------------------------------------------------
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

-- EJERCICIO 6 -----------------------------------------------------------------
esAptoReverso :: Mensaje -> Bool
esAptoReverso m = extraerMensajeParaEnvio m /= extraerMensajeParaEnvio (cifrarReverso m)

-- Ejemplo función esAptoReverso
--
-- *Main> esAptoReverso (TextoClaro "SOPAPOS")
-- False
-- it :: Bool
--
-- *Main> esAptoReverso (TextoClaro "SOPA")
-- True
-- it :: Bool



-- EJERCICIO 8 ----------------------------------------------------------------
-- Función cifradoCesar: Dado un Mensaje y un Desplazamiento, devuelve un Mensaje
-- encriptado con el cifrado Cesar (con el dezplazamiento indicado).

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
                   | otherwise = cifrarCaracter c (mod n 26)

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
-- Función cifrarPalabrasReverso: Dado un Mensaje, me lo cifra con el método del
-- cifrado de las Palabras Reverso.

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
listaPalabras s [] e = derecha s e : [] -- Esto es la última palabra de un texto
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

-- Ejemplo función cifrarPalabrasReverso
--
-- *Main> cifrarPalabrasReverso (TextoClaro "EN EL CUARTO OSCURO")
-- CifradoPalabrasReverso (TextoClaro "NE LE OTRAUC ORUCSO")
-- it :: Mensaje
--
-- *Main> cifrarPalabrasReverso (cifrarCesar (cifrarReverso (TextoClaro "EN EL CUARTO OSCURO")) 2)
-- CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "QUEWTQ EWCTVQ GN GP")) 2)
-- it :: Mensaje
