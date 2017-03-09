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
-- Sin cambios respecto de la Parte 1.

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


-- EJERCICIO 2 -----------------------------------------------------------------
-- Sin cambios respecto de la Parte 1, ya que la función me sigue percibiendo
-- cualquier Mensaje, que no sea TextoClaro, como mensaje cifrado.

esMensajeCifrado :: Mensaje -> Bool
esMensajeCifrado (TextoClaro t) = False
esMensajeCifrado _ = True

-- Ejemplo de la función esMensajeCifrado
--
-- *Main> esMensajeCifrado (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- True
-- it :: Bool


-- EJERCICIO 3 -----------------------------------------------------------------
-- Se realizó una extención de la función cifrarReverso, de modo que acepte también
-- Mensajes de los tipos CifradoCesar y CifradoPalabrasReverso.

reverso :: Texto -> Texto
reverso [] = []
reverso (t:ts) = reverso ts ++ [t]

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro t) = CifradoReverso (TextoClaro (reverso t))
cifrarReverso (CifradoReverso m) = CifradoReverso (cifrarReverso m)
cifrarReverso (CifradoCesar m n) = CifradoCesar (cifrarReverso m) n
cifrarReverso (CifradoPalabrasReverso m) = CifradoPalabrasReverso (cifrarReverso m)

-- Ejemplo función cifrarReverso extendida
--
-- *Main> cifrarReverso (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- CifradoReverso (CifradoCesar (CifradoPalabrasReverso (CifradoReverso (TextoClaro "CNQJ UCNQTCV"))) 2)
-- it :: Mensaje


-- EJERCICIO 4 -----------------------------------------------------------------
-- Se realizó una extención de la función extraerMensajeParaEnvio, de modo que acepte también
-- Mensajes de los tipos CifradoCesar y CifradoPalabrasReverso.

extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro t) = t
extraerMensajeParaEnvio (CifradoReverso m) = extraerMensajeParaEnvio m
extraerMensajeParaEnvio (CifradoCesar m n) = extraerMensajeParaEnvio m
extraerMensajeParaEnvio (CifradoPalabrasReverso m) = extraerMensajeParaEnvio m

-- Ejemplo función extraerMensajeParaEnvio extendida
--
-- *Main> extraerMensajeParaEnvio (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- "VCTQNCU JQNC"
-- it :: Texto


-- EJERCICIO 5 -----------------------------------------------------------------
-- Se realizó una extención de la función descifrar, de modo que desencripte también
-- Mensajes de los tipos CifradoCesar y CifradoPalabrasReverso.

descifrar :: Mensaje -> Texto
descifrar (TextoClaro t) = t
descifrar (CifradoReverso m) = reverso (descifrar m)
descifrar (CifradoCesar m n) = cifrarTexto (descifrar m) (-n)
descifrar (CifradoPalabrasReverso m) = reversoPalabras (descifrar m)

-- Ejemplo función descifrar extendida
--
-- *Main> descifrar (CifradoReverso (CifradoCesar (CifradoPalabrasReverso (TextoClaro "VCTQNCU JQNC")) 2))
-- "HOLA TAROLAS"
-- it :: Texto


-- EJERCICIO 6 -----------------------------------------------------------------
-- Sin cambios respecto de la Parte 1. Esta definición ya funciona para el tipo
-- extendido.

esAptoReverso :: Mensaje -> Bool
esAptoReverso m = extraerMensajeParaEnvio m /= extraerMensajeParaEnvio (cifrarReverso m)

-- Ejemplo de la función esAptoReverso
--
-- *Main> esAptoReverso (CifradoCesar (TextoClaro "VMCYCMV") 8)
-- False
--
-- *Main> esAptoReverso (CifradoPalabrasReverso (TextoClaro "NEUQUEN NEUQUEN"))
-- False



-- EJERCICIO 8 ----------------------------------------------------------------
-- Función cifradoCesar: Dado un Mensaje y un Desplazamiento, devuelve un Mensaje
-- encriptado con el cifrado Cesar (con el dezplazamiento indicado).

-- Función auxiliar siguiente: dado un caracter, devuelve el caracter siguiente.
-- Es funcion auxiliar de cifrarCaracter.
siguiente :: Char -> Char
siguiente 'Z' = 'A'
siguiente ' ' = ' '
siguiente c = succ c

-- Función auxiliar cifrarCaracter: dado un caracter y un Desplazamiento,
-- devuelve el caracter que resulta de la aplicacion de dicho desplazamiento.
-- Es función auxiliar de cifrarTexto.
cifrarCaracter :: Char -> Desplazamiento -> Char
cifrarCaracter c 0 = c
cifrarCaracter c 1 = siguiente c
cifrarCaracter c n | n > 0 = siguiente (cifrarCaracter c ((mod n 26)-1))
                   | otherwise = cifrarCaracter c (mod n 26)

-- Función auxiliar cifrarTexto: dado un texto y un Desplazamiento, desplaza
-- cada caracter del texto de forma recursiva según el Desplazamiento indicado.
-- Es función auxiliar de cifrarCesar.
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

-- Función auxiliar posicionesEspacios: dado un Texto y un Integer que
-- representa el valor para la posicion del primer caracter del texto, devuelve
-- las posiciones de los espacios.
-- Es funcion auxiliar de reversoPalabras.
posicionesEspacios :: Texto -> Integer -> [Integer]
posicionesEspacios [] n = []
posicionesEspacios (' ':cs) n = n : posicionesEspacios cs (n+1)
posicionesEspacios (c:cs) n = posicionesEspacios cs (n+1)

-- Función auxiliar derecha: dado un Texto y un Integer, recorta la parte
-- izquierda del texto para que comience justo después de la posicion indicada
-- por el integer. Es decir, se queda con la parte derecha.
-- Asume que la primera posicion es 0.
-- Es funcion auxiliar de listaPalabras
derecha :: Texto -> Integer -> Texto
derecha (c:cs) 0 = cs
derecha (c:cs) n = derecha cs (n-1)

-- Función auxiliar izquierda: dado un Texto y un Integer, recorta la parte
-- derecha del texto justo antes de la posición indicada por el Integer. Es
-- decir, se queda con la parte izquierda.
-- Asume que la primera posicion es 0.
-- Es funcion auxiliar de listaPalabras
izquierda :: Texto -> Integer -> Texto
izquierda s 0 = []
izquierda (c:cs) n = c : izquierda cs (n-1)

-- Función auxiliar listaPalabras: dado un Texto, una lista de integers
-- representando las posiciones de los espacios del Texto y un Integer
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

-- Función auxiliar reversoListaPalabras: dada una lista de palabras, revierte
-- cada una de ellas.
-- Es funcion auxiliar de reversoPalabras
reversoListaPalabras :: [Texto] -> [Texto]
reversoListaPalabras [] = []
reversoListaPalabras (p:ps) = reverso p : reversoListaPalabras ps

-- Función auxiliar listaPalabrasATexto: dada una lista de palabras genera un
-- Texto con las palabras de la lista separadas por espacios.
-- Es funcion auxiliar de reversoPalabras
listaPalabrasATexto :: [Texto] -> Texto
listaPalabrasATexto (p:[]) = p
listaPalabrasATexto (p:ps) = p ++ " " ++ listaPalabrasATexto ps

-- Función auxiliar reversoPalabras: dado un texto, devuelve cada palabra del
-- texto al revés.
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
