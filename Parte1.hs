-- Octavio, Gianatiempo - 280/10 - ogianatiempo@gmail.com
-- Jorge, Mamani Fernandez - 23/08 - papelyto_god@hotmail.com

{-
PENDIENTES:
- Agregar ejemplos
- Chequear que los nombres de las funciones esten bien
- Comentarios explicando las funciones auxiliares
- Volver a chequear si andan las funciones con el tipo para la Parte2
- Adaptar cifrar reverso para que tome cesar y palabrasreverso
-}

-- Lo hacemos modulo para importarlo en la parte2 -----------------------------
module Parte1
  where

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


-- Ejercicio 1 ----------------------------------------------------------------
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


-- Ejercicio 2 ----------------------------------------------------------------
-- Función esMensajeCifrado: dado un Mensaje devuelve True si y sólo si el
-- Mensaje ya ha sido cifrado.

esMensajeCifrado :: Mensaje -> Bool
esMensajeCifrado (TextoClaro t) = False
esMensajeCifrado _ = True

-- Ejercicio 3 ----------------------------------------------------------------
-- Función cifrarReverso: dado un Mensaje, lo encripta con el cifrado reverso.

reverso :: Texto -> Texto
reverso [] = []
reverso (t:ts) = reverso ts ++ [t]

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro t) = CifradoReverso (TextoClaro (reverso t))
cifrarReverso (CifradoReverso m) = CifradoReverso (cifrarReverso m)
cifrarReverso (CifradoCesar m n) = CifradoCesar (cifrarReverso m) n
cifrarReverso (CifradoPalabrasReverso m) = CifradoPalabrasReverso (cifrarReverso m)

-- Ejercicio 4 ----------------------------------------------------------------
--  Ya está adaptado para Parte2

extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro t) = t
extraerMensajeParaEnvio (CifradoReverso m) = extraerMensajeParaEnvio m
extraerMensajeParaEnvio (CifradoCesar m n) = extraerMensajeParaEnvio m
extraerMensajeParaEnvio (CifradoPalabrasReverso m) = extraerMensajeParaEnvio m

-- Ejercicio 5 ----------------------------------------------------------------
-- Función descifrar: dado un Mensaje nos permite recuperar el Texto que
-- contiene la información que fue ocultada.

descifrar :: Mensaje -> Texto
descifrar (TextoClaro t) = t
descifrar (CifradoReverso m) = reverso (descifrar m)

-- Ejercicio 6 ----------------------------------------------------------------
-- función esAptoReverso: dado un Mensaje, devielve True si el cifrado reverso
-- tiene sentido ser aplicado, es decir, si complica un poco la lectura del
-- mensaje.

esAptoReverso :: Mensaje -> Bool
esAptoReverso m = extraerMensajeParaEnvio m /= extraerMensajeParaEnvio (cifrarReverso m)
