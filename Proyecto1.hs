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

