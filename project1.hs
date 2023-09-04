{-
Algoritmos y Estructuras de Datos I 
Laboratorio Proyecto 1

Ejercicios
-}

--1)
--a)
esCero :: Int -> Bool
esCero n = n==0

{-
> esCero 5
> False

> esCero 0
> True
-}

--b)
esPositivo :: Int -> Bool 
esPositivo n = n > 0

{-
> esPositivo 3
> True

> esPositivo (-6)
> False
-}

--c)
esVocal :: Char -> Bool
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

{-
> esVocal 'r'
> False

> esVocal 'e'
> True
-}

--d)
valorAbsoluto :: Int -> Int
valorAbsoluto n
  | n >= 0 = n
  | n < 0 = -n

{-
> valorAbsoluto 10
> 10

> valorAbsoluto (-25)
> 25
-}

--2)
--a)
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs

{-
> paratodo [True, True]
> True

> paratodo [True, False, True]
> False
-}

--b)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

{-
> sumatoria [2, 3, 4, 5]
> 14

> sumatoria [2, 6, 5]
> 13
-}

--c)
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

{-
> productoria [2, 4, 2]
> 16

> productoria [4, 4, 2]
> 32
-}

--d)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

{-
> factorial 5
> 120

> factorial 10
> 3628800
-}

--e)
promedio :: [Int] -> Int
promedio xs = sumatoria xs `div` length xs

{-
> promedio [2, 4, 5]
> 3

> promedio [5, 7, 6, 9]
> 6
-}

--3)
pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs 

{-
> pertenece 3 [1, 2, 3, 4]
> True

> pertenece 5 [1, 2, 3, 4]
> False
-}

--4)
--a)
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) t = t x && paratodo' xs t 

{-
> paratodo' [2, 4, 6, 8] even
> True

> paratodo' [2, 3, 6, 8] even
> False
-}

--b)
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False
existe' (x:xs) t = t x || existe' xs t 

{-
> existe' [2, 4, 6, 8] odd
> False

> existe' [2, 3, 6, 8] odd
> True
-}

--c)
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t

{-
> sumatoria' [1, 2, 3, 4] (*2)
> 20

> sumatoria' [2, 4, 6, 8] (*2)
> 40
-}

--d)
productoria' :: [a] -> (a ->Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) t = t x * productoria' xs t

{-
> productoria' [1, 2, 3, 4] (*2)
> 384

> productoria' [2, 4, 6, 8] (*2)
> 6144
-}

--5)
paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs id

{-
> paratodo'' [True, True, True]
> True

> paratodo'' [True, False, True]
> False
-}

--6)
--a)
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even

{-
> todosPares [2, 4, 6, 8]
> True

> todosPares [2, 3, 6, 8]
> False
-}

--b)
esMultiplo :: Int -> Int -> Bool
esMultiplo n m = n `mod` m == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (`esMultiplo` n)

{-
> hayMultiplo 3 [1, 2, 4, 9, 11]
> True

> hayMultiplo 5 [2, 7, 11, 16, 22]
> False
-}

--c)
sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n-1] (^2)

{-
> sumaCuadrados 5
> 30

> sumaCuadrados 3
> 5
-}

--d)
existeDivisor :: Int -> [Int] -> Bool
existeDivisor n ls = existe' ls (esMultiplo n)

{-
> existeDivisor 7 [2, 3, 4, 5]
> False

> existeDivisor 8 [2, 3, 5]
> True
-}

--e)
esPrimo :: Int -> Bool
esPrimo n = not (existeDivisor n [2..n-1]) && n>=2

{-
> esPrimo 7
> True

> esPrimo 8
> False
-}

--f)
factorial' :: Int -> Int
factorial' n = productoria [1..n] 

{-
> factorial' 5
> 120

> factorial' 10
> 3628800
-}

--g)
multiplicaPrimos :: [Int] -> Int 
multiplicaPrimos xs = productoria (filter esPrimo xs)

{-
> multiplicaPrimos [2, 3, 4, 5]
> 30

> multiplicaPrimos [7, 8, 9, 10]
> 7
-}

--h)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


esFib :: Int -> Bool
esFib n = pertenece n (map fib [0..n+2])

{-

> esFib 3
> True

> esFib 6
> False
-}

--i)
todosFib :: [Int] -> Bool
todosFib xs = paratodo' xs esFib

{-
> todosFib [0, 1, 2, 3, 5]
> True

> todosFib [0, 1, 2, 4, 7]
> False

-}

{-
7)
La función Map aplica una función a cada elemento de una lista y devuelve una nueva lista con los resultados obtenidos.
La función Filter ignora los elementos de una lista que no cumplan una condición dada y devuelve una nueva lista con los elementos que sí cumplen.

/map succ [1, -4, 6, 2, -8]/ suma 1 a cada número en la lista,
> resultando en [2, -3, 7, 3, -7].

/filter esPositivo [1, -4, 6, 2, -8]/ deja pasar solo los números positivos en la lista, 
> dando como resultado [1, 6, 2].

-}

--8)
--a)
duplicarTodos :: [Int] -> [Int]
duplicarTodos [] = []
duplicarTodos (x:xs) = x*2 : duplicarTodos xs

{-
> duplicarTodos [1, 2, 3, 4, 5]
> [2, 4, 6, 8, 10]

> duplicarTodos [0, -1, 10]
> [0, -2, 20]
-}

--b)
duplicarTodos' :: [Int] -> [Int]
duplicarTodos' xs = map (*2) xs

{-
> duplicarTodos' [1, 4, 3, 7, 5]
> [2, 8, 6, 14, 10]

> duplicarTodos' [0, 76, 10]
> [0, 152, 20]
-}

--9)
--a)
filtrarPrimos :: [Int] -> [Int]
filtrarPrimos [] = []
filtrarPrimos (x:xs) 
  | esPrimo x = x : filtrarPrimos xs
  | otherwise = filtrarPrimos xs

{-
> filtrarPrimos [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
> [2, 3, 5, 7]

> filtrarPrimos [15, 25, 30, 35]
> []
-}

--b)
filtrarPrimos' :: [Int] -> [Int]
filtrarPrimos' xs = filter esPrimo xs

{-
> filtrarPrimos' [1, 2, 3, 4, 6, 7, 8, 9]
> [2, 3, 7]

> filtrarPrimos' [30, 35]
> []
-}

--c)
multiplicaPrimos' :: [Int] -> Int 
multiplicaPrimos' xs = productoria (filtrarPrimos' xs)

{-
> multiplicaPrimos' [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
> 210

> multiplicaPrimos' [11, 13, 17, 19]
> 46189
-}

--10)
--a)
primIgualesA :: Ord a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA n (x:xs) 
  | n == x = n : primIgualesA n xs
  | otherwise = []

{-
> primIgualesA 3 [3, 3, 4, 1, 3, 3]
> [3, 3]

> primIgualesA 'a' "aabbcde"
> "aa"
-}

--b)
primIgualesA' :: Ord a => a -> [a] -> [a]
primIgualesA' n xs = takeWhile ( == n) xs

{-
> primIgualesA' 3 [3, 4, 1, 3, 3]
> [3]

> primIgualesA' 'a' "eeeaabbcde"
> "eee"
-}

--11)
--a)
primIguales :: Ord a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs)
  | x == y = x: primIguales (y:xs)
  | otherwise = [x]

{-
> primIguales [4, 1, 3, 3]
> [4]

> primIguales "hhhaabbcde"
> "hhh"
-}

--b)
primIguales' :: Ord a => [a] -> [a]
primIguales' [] = []
primIguales' (x:xs) = primIgualesA' x (x:xs)

{-
> primIguales' [1, 3, 3]
> [1]

> primIguales' "jjbcde"
> "jj"
-}