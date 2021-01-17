--Ejercicio 1

--esCero
esCero :: Int -> Bool
esCero x | x == 0 = True
         | otherwise = False

--esPositivo
esPositivo :: Int -> Bool 
esPositivo x | x > 0 = True 
             | otherwise = False

--esVocal
esVocal :: Char -> Bool 
esVocal n =  n `elem` "aeiou"

--Ejercicio 2

--a) paraTodo
paratodo :: [Bool] -> Bool
paratodo [True] = True
paratodo [] = False
paratodo (x:xs) | (x==True) = paratodo xs
                | (x==False) = False 

--b) sumatoria
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--Sin usar recursión, sólo para probar de otra manera
sumatoria' :: [Int] -> Int
sumatoria' xs = foldr (+) 0 xs

--c) productoria
productoria :: [Int] -> Int 
productoria [] = 1
productoria (x:xs) = x * productoria xs 

--Sin usar recursión, sólo para probar de otra manera
productoria' :: [Int] -> Int
productoria' xs = foldr (*) 1 xs

--d) factorial
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1 
factorial x = x * factorial(x-1)


--e) promedio
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = sumatoria (x:xs) `div` length (x:xs)


--Ejercicio 3

--pertenece
pertenece :: Int -> [Int] -> Bool
pertenece _ []       = False
pertenece x (y:ys)   = x==y || elem x ys


--Ejercicio 4

--paratodo'

paratodo' :: [a] -> (a->Bool) -> Bool 
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t

--existe

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t

--sumatoria

sumatoria' :: [a] -> (a -> Int) -> Int 
sumatoria' [] t = 0
sumatoria' (x:xs) t = (t x) + sumatoria' xs t

--productoria

productoria' :: [a] -> (a -> Int) -> Int 
productoria' [] t = 1
productoria' (x:xs) t = (t x) * productoria' xs t


--Ejercicio 5

paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs id

--Ejercicio 6

--todosPares

todosPares :: [Int] -> Bool 
todosPares xs = paratodo' xs even

--hayMultiplo


--funcion auxiliar
esMultiplo :: Int -> Int -> Bool 
esMultiplo 0 0 = False
esMultiplo x y | (x `mod` y == 0) = True 
               | otherwise = False 

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo x xs = existe' xs (esMultiplo x)

--SumaCuadrados
 
sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [x*x | x <- [1..x]] id

--factorial'

factorial2 :: Int -> Int
factorial2 x = productoria' [x | x <- [1..x]] id

--multiplicaPares
--funcion aux
esPar :: Int -> Int
esPar (x) | even x = x
          | otherwise = 1
                 
multiplicaPares :: [Int] -> Int
multiplicaPares xs =  productoria' xs esPar

--Ejercicio 7

--Punto a) La función map, toma una función, una lista, y devuelve una nueva lista con las modificaciones correspondientes
--la función filter, toma un predicado y una lista, y devuelve una nueva lista con los datos que han podido satisfacer el predicado

--punto b)
map succ [1,-4,6,2,-8]

      >> [2,-3,7,3,-7]

--punto c)

filter esPositivo [1,-4,6,2,-8]

               >> [1,6,2]


--Ejercicio 8
--recursión
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = 2*x : duplica xs

--map
duplica :: [Int] -> [Int]
duplica xs = map (2*) xs


--Ejercicio 9
--Recursión
pares :: [Int] -> [Int]
pares [] = [] 
pares (x:xs) | (x `mod` 2 == 0) = x: pares xs
             | otherwise = pares xs

--map
pares' :: [Int] -> [Int]
pares' xs = filter even xs

--Optimización del ejercicio 6)e

--Antes
esPar :: Int -> Int
esPar (x) | even x = x
          | otherwise = 1
                 
multiplicaPares :: [Int] -> Int
multiplicaPares xs =  productoria' xs esPar

--Ahora
multiplicaPares' :: [Int] -> Int
multiplicaPares' xs =  productoria (filter even xs)

--Ejercicio 10

primIgualesA :: (Eq a) => a -> [a] -> [a]
primIgualesA  _ [] = []
primIgualesA x (y:ys) | (y == x) = y : primIgualesA x ys
                      | otherwise = []


primIgualesA' :: (Eq a) => a -> [a] -> [a]
primIgualesA' y xs = takeWhile (==y) xs


--Ejercicio 11

primIguales :: (Eq a) =>[a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:(y:xs)) | x == y = x: primIguales (y:xs)
                       | otherwise = x: []


primIguales' :: (Eq a) => [a] -> [a]
primIguales' (y:(x:xs)) = takeWhile (==y) (x:xs)