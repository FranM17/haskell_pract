borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

collect :: (Eq k) => [(k, v)] -> [(k, [v])]
collect [] = []
collect ((k,v):xs) = collect' k [v] xs
    where
        collect' key vals [] = [(key, reverse vals)]
        collect' key vals ((k2,v2):xs)
            | key == k2  = collect' key (v2:vals) xs
            | otherwise  = (key, reverse vals) : collect' k2 [v2] xs

serie :: [a] -> [[a]]
serie [] = [[]]
serie xs = (serie (init xs)) ++ [xs]

paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d = go a b c d 3
    where 
        go a b c d z 
            | a == b && c == d = True
            | z == 0 = False
            | otherwise = go a d b c (z-1)

isosceles :: Int -> Int -> Int -> Bool
isosceles a b c = go a b c 3
    where 
        go a b c z 
            | a == b = True
            | z == 0 = False
            | otherwise = go c a b (z-1)

ror :: [a] -> Int -> [a]
ror [] _ = []
ror xs 0 = xs
ror xs n 
    | ((length xs) - n) >= 0 = go xs ((length xs) - n)
    | otherwise = xs
    where
        go xs 0 = xs
        go xs n = go ((last xs) : (init xs)) (n-1)


{-
        go _ 0 list = list
        go xs n list 

        
            | (n-1) == 0 = 


    | n <= (length xs) = ror tail xs ++ [head xs]
    | otherwise = xs
-}

upto :: Int -> Int -> [Int]
upto n m = go n m []
    where   
        go n m xs
            | n > m = xs
            | otherwise = go (n+1) m (xs ++ [n]) 

--  eco "hola" = "hoolllaaaa"
eco :: [a] -> [a]
eco xs = go xs 0 0 (length xs) []
    where
        go xs cont conttt l ys
            | cont == l = ys
            | conttt <= cont = go xs cont (conttt+1) l (ys ++ [head xs])
            | otherwise = go (tail xs) (cont+1) 0 l ys

--[x | x <- xs, xs <- [x | x <- [2..(n-1)], n ´mod´ x == 0], x < (map sum xs)]
--abundantes :: [Integer] 
--abundantes = [x | x <- div, div <- [y | y <- [2..(n-1)], n ´mod´ y == 0], n <- [1..10], x < (map sum div)]



