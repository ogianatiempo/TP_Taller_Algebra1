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
