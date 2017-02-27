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

-- Definicion de los tipos ----------------------------------------------------
-- Renombre para tipo Texto
type Texto = [Char]

-- Definición del tipo recursivo Mensaje
data Mensaje = TextoClaro Texto
              | CifradoReverso Mensaje
              deriving (Eq, Show)

-- Mensajes para probar las funciones: ----------------------------------------
m1 :: Texto
m1 = "TENGA USTED BUENAS TARDES"

m2 :: Texto
m2 = "SIEMPRE REVERSO"

m3 :: Texto
m3 = "NEUQUEN"

m4 :: Texto
m4 = "ANANA"

m5 :: Texto
m5 = "OJOTA"


-- Ejercicio 1 ----------------------------------------------------------------
-- Funcion crearMensaje: dado un Texto devuelve un Mensaje en TextoClaro
-- Debe chequear que el Texto este compuesto de cualquier elemento de la lista
-- de la A a la Z y espacios. Sino se debe indefinir

-- Chequearia recursivamente si cada elemento del Texto pertenece a la lista
-- ' ':['A'..'Z']

-- crearMensaje :: Texto -> Mensaje


-- Ejercicio 2 ----------------------------------------------------------------
-- Función esMensajeCifrado: dado un Mensaje devuelve True si y sólo si el
-- Mensaje ya ha sido cifrado.


-- Ejercicio 3 ----------------------------------------------------------------
-- Función cifrarReverso: dado un Mensaje, lo encripta con el cifrado reverso.


-- Ejercicio 4 ----------------------------------------------------------------
-- Función extraerMensajeParaEnvio: dado un Mensaje devuelve un Texto listo
-- para ser enviado que se corresponde con el mensaje cifrado.


-- Ejercicio 5 ----------------------------------------------------------------
-- Función descifrar: dado un Mensaje nos permite recuperar el Texto que
-- contiene la información que fue ocultada.

-- Ejercicio 6 ----------------------------------------------------------------
-- función esAptoReverso: dado un Mensaje, devielve True si el cifrado reverso
-- tiene sentido ser aplicado, es decir, si complica un poco la lectura del
-- mensaje.
