-- Octavio, Gianatiempo - 280/10 - ogianatiempo@gmail.com
-- Jorge, Mamani Fernandez - 23/08 - papelyto_god@hotmail.com



-- Definicion de los tipos ----------------------------------------------------
-- Renombre para tipo Texto
type Texto = [Char]

-- Definición del tipo recursivo Mensaje
data Mensaje = TextoClaro Texto | CifradoReverso Mensaje   deriving (Eq, Show)



-- EJERCICIO 1 ----------------------------------------------------------------
-- Funcion crearMensaje: dado un Texto devuelve un Mensaje en TextoClaro
-- Debe chequear que el Texto este compuesto de cualquier elemento de la lista
-- de la A a la Z y espacios. Sino se debe indefinir

-- Función auxiliar pertenece: Dado un Char (caracter) y un String (lista de
-- caracteres), me indica si el caracter pertenece o no a la lista (True o False).
pertenece :: Char -> String -> Bool
pertenece c [] = False
pertenece c (l:ls) = c == l || pertenece c ls

-- Función auxiliar esCaracterPermitido: Dado un caracter, me indica si el
-- caracter es un caracter permitido. Es considerado permitido si el caracter es
-- el espacio o algunas de las 26 letras mayúsculas del abecedario.
esCaracterPermitido :: Char -> Bool
esCaracterPermitido c = pertenece c (' ':['A'..'Z'])

-- Función auxiliar esTextoPermitido: Dado un Texto, devuelve True si el texto
-- está formado pura y exclusivamente por los caracteres permitidos.
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

-- Ejemplos Función esMensajeCifrado
--
-- *Main> esMensajeCifrado (TextoClaro "HOLA TAROLAS")
-- False
-- it :: Bool
--
-- *Main> esMensajeCifrado (CifradoReverso (TextoClaro "SALORAT ALOH"))
-- True
-- it :: Bool



-- EJERCICIO 3 ----------------------------------------------------------------
-- Función cifrarReverso: Dado un Mensaje, lo encripta con el cifrado reverso.

-- Función auxiliar reverso: Dado Texto, me devuelve el texto invertido.
reverso :: Texto -> Texto
reverso [] = []
reverso (t:ts) = reverso ts ++ [t]

cifrarReverso :: Mensaje -> Mensaje
cifrarReverso (TextoClaro t) = CifradoReverso (TextoClaro (reverso t))
cifrarReverso (CifradoReverso m) = CifradoReverso (cifrarReverso m)

-- Ejemplos función cifrarReverso
--
-- *Main> cifrarReverso (TextoClaro "EL POSTRE")
-- CifradoReverso (TextoClaro "ERTSOP LE")
--
-- *Main> cifrarReverso (cifrarReverso (TextoClaro "EL POSTRE"))
-- CifradoReverso (CifradoReverso (TextoClaro "EL POSTRE"))



-- EJERCICIO 4 ----------------------------------------------------------------
-- Función extraerMensajeParaenvio: dado un Mensaje nos develve un Texto listo
-- para ser enviado, que se corresponde con el mensaje ya cifrado.

extraerMensajeParaEnvio :: Mensaje -> Texto
extraerMensajeParaEnvio (TextoClaro t) = t
extraerMensajeParaEnvio (CifradoReverso m) = extraerMensajeParaEnvio m

-- Ejemplos Función extraerMensajeParaEnvio
--
-- *Main> extraerMensajeParaEnvio (TextoClaro "EL POSTRE")
-- "EL POSTRE"
-- it :: Texto
--
-- *Main> extraerMensajeParaEnvio (CifradoReverso (TextoClaro "ERTSOP LE"))
-- "ERTSOP LE"
-- it :: Texto
--



-- EJERCICIO 5 ----------------------------------------------------------------
-- Función descifrar: dado un Mensaje nos permite recuperar el Texto que
-- contiene la información que fue oculta.

descifrar :: Mensaje -> Texto
descifrar (TextoClaro t) = t
descifrar (CifradoReverso m) = reverso (descifrar m)

-- Ejemplos función descifrar
--
-- *Main> descifrar (CifradoReverso (TextoClaro "ERTSOP LE"))
-- "EL POSTRE"
-- it :: Texto
--
-- *Main> descifrar (cifrarReverso (cifrarReverso (TextoClaro "HOLA TAROLAS")))
-- "HOLA TAROLAS"
-- it :: Texto



-- EJERCICIO 6 ----------------------------------------------------------------
-- función esAptoReverso: dado un Mensaje, devielve True si el cifrado reverso
-- tiene sentido ser aplicado, es decir, si complica un poco la lectura del
-- mensaje.

esAptoReverso :: Mensaje -> Bool
esAptoReverso m = extraerMensajeParaEnvio m /= extraerMensajeParaEnvio (cifrarReverso m)

-- Ejemplos función esAptoReverso
--
-- *Main> esAptoReverso (TextoClaro "SOPAPOS")
-- False
-- it :: Bool
--
-- *Main> esAptoReverso (TextoClaro "SOPA")
-- True
-- it :: Bool
