module Practica01 where
--Funcioneas auxiliares

--Es la funcion que veifica que un número entero sea par (fijandose en el residuo al dividir entre 2)
-- se usa en el ejercicio 1 de Relaciones (relacionDivisor)
esParI :: Int -> Bool
esParI x = x `mod` 2 == 0

--Es la funcion que verififica que un número sea multiplo de 5 fijandose en el residuo
-- se usa en el ejercicio 2 de Relaciones (relacionSumaEspecial)
esMulti5 :: Int -> Bool
esMulti5 x = x `mod` 5 == 0

--FUNCIONES
valorAbs :: Int -> Int
valorAbs n = if n < 0 then (-n) else n


esDivisor :: Int -> Int -> Bool
esDivisor a b =
  if a == 0
     then False
     else (b `mod` a) == 0

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v = a * v * v + b * v + c


sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (p1, q1) (p2, q2) =
  if q1 == q2
     then (p1 + p2, q1)
     else (p1 * q2 + p2 * q1, q1 * q2)

comparador :: Float -> Float -> Int
comparador x y = if x == y then 0 else (if x < y then -1 else 1)

puntoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
puntoMedio (x1, y1) (x2, y2) =
  ( (x1 + x2) / 2 , (y1 + y2) / 2 )


--RELACIONES
type Rel a b = [(a, b)]

relacionDivisor :: Rel Int Int
relacionDivisor = [(x, y) | x <- [1..30], y <- [1..30], (esParI x == esParI y) && esDivisor x y]

relacionSumaEspecial :: Rel Int Int
relacionSumaEspecial = [(x, y) | x <- [1..30], y <- [1..30], esMulti5 (x+y) && x < y]

relacionCongruentesModuloN :: Int -> Rel Int Int
relacionCongruentesModuloN n = [(x, y) | x <- [1..30], y <- [1..30], x `mod` n == y `mod` n && x /= y]


--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

esPar :: Natural -> Bool
esPar Cero         = True
esPar (Suc Cero)   = False
esPar (Suc (Suc n)) = esPar n

iguales :: Natural -> Natural -> Bool
iguales Cero Cero         = True
iguales Cero (Suc _)      = False
iguales (Suc _) Cero      = False
iguales (Suc n) (Suc m) = iguales n m

maximo :: Natural -> Natural -> Natural 
maximo Cero n = n
maximo n Cero = n
maximo (Suc n) (Suc m) = Suc (maximo n m)


potencia :: Natural -> Natural -> Natural
potencia n Cero = (Suc Cero) 
potencia  n (Suc m) = multiplicacion n (potencia n m)

multiplicacion :: Natural -> Natural -> Natural
multiplicacion Cero n = Cero
multiplicacion n Cero = Cero
multiplicacion (Suc n) (Suc m) = suma (multiplicacion (Suc n) m) (Suc n)

suma :: Natural -> Natural -> Natural
suma Cero n = n
suma n Cero = n
suma (Suc n) m  = Suc (suma n m)