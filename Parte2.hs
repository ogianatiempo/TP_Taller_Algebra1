-- Octavio, Gianatiempo - 280/10 - ogianatiempo@gmail.com
-- Jorge, Mamani Fernandez - 23/08 - papelyto_god@hotmail.com

-- Definicion de los tipos ----------------------------------------------------
-- Renombre para tipo Texto
type Texto = [Char]

-- Renombre para tipo Desplazamiento, es para la Parte2
type Desplazamiento = Integer

-- Definición del tipo recursivo Mensaje, sirve también para la Parte2
data Mensaje = TextoClaro Texto
               | CifradoReverso Mensaje
               | CifradoCesar Mensaje Desplazamiento
               | CifradoPalabrasReverso Mensaje
               deriving (Eq, Show)

-- Textos para probar las funciones: ----------------------------------------
t1 :: Texto
t1 = "TENGA USTED BUENAS TARDES"

t2 :: Texto
t2 = "SIEMPRE REVERSO"

t3 :: Texto
t3 = "NEUQUEN"

t4 :: Texto
t4 = "ANANA"

t5 :: Texto
t5 = "OJOTA"

t6 :: Texto
t6 = "LINEA C RETIRO CONSTITUCION "

t7 :: Texto
t7 = "UNO DOS TRES PROBANDO"


-- EJERCICIO 1 ----------------------------------------------------------------
-- Funcion crearMensaje: dado un Texto devuelve un Mensaje en TextoClaro
-- Debe chequear que el Texto este compuesto de cualquier elemento de la lista
-- de la A a la Z y espacios. Sino se debe indefinir


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
-- *** Exception: Parte1.hs:72:1-50: Non-exhaustive patterns in function crearMensaje


-- EJERCICIO 2 ----------------------------------------------------------------
-- Función esMensajeCifrado: dado un Mensaje devuelve True si y sólo si el
-- Mensaje ya ha sido cifrado.

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



-- EJERCICIO 3 ----------------------------------------------------------------
-- Función cifrarReverso: dado un Mensaje, lo encripta con el cifrado reverso.

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


-- EJERCICIO 4 ----------------------------------------------------------------
--  Ya está adaptado para Parte2

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


-- EJERCICIO 5 ----------------------------------------------------------------
-- Función descifrar: dado un Mensaje nos permite recuperar el Texto que
-- contiene la información que fue ocultada.

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

-- EJERCICIO 6 ----------------------------------------------------------------
-- función esAptoReverso: dado un Mensaje, devielve True si el cifrado reverso
-- tiene sentido ser aplicado, es decir, si complica un poco la lectura del
-- mensaje.

-- Se podría hacer una función análoga para CifradoPalabrasReverso

esAptoReverso :: Mensaje -> Bool
esAptoReverso m = extraerMensajeParaEnvio m /= extraerMensajeParaEnvio (cifrarReverso m)

-- Ejemplo función esAptoReverso
--
-- *Main> esAptoReverso (TextoClaro "sopapos")
-- False
-- it :: Bool
--
-- *Main> esAptoReverso (TextoClaro "sopa")
-- True
-- it :: Bool

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

-- Ejemplo función cifrarPalabrasReverso
--
-- *Main> cifrarPalabrasReverso (TextoClaro "EN EL CUARTO OSCURO")
-- CifradoPalabrasReverso (TextoClaro "NE LE OTRAUC ORUCSO")
-- it :: Mensaje
--
-- *Main> cifrarPalabrasReverso (cifrarCesar (cifrarReverso (TextoClaro "EN EL CUARTO OSCURO")) 2)
-- CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "QUEWTQ EWCTVQ GN GP")) 2)
-- it :: Mensaje
--
-- *Main> extraerMensajeParaEnvio (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "QUEWTQ EWCTVQ GN GP")) 2))
-- "QUEWTQ EWCTVQ GN GP"
-- it :: Texto
