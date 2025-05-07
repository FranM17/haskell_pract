map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f xs = foldr (\x ys -> (f x) : ys) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f xs = foldr (\x ys -> if f x then x : ys else ys) [] xs

unzip' ::[(a, b)] -> ([a], [b])
unzip' xs = foldr (\(a,b) (as, bs) -> (a : as, b : bs)) ([], []) xs

--Ej. pair2List (x , [y1 , y2 , y3 ]) = [(x , y1 ),(x , y2 ),(x , y3 )]
pair2List ::(a, [b]) -> [(a, b)]
pair2List (x, ys) = foldr (\y xs -> (x,y) : xs) [] ys

--Ej.maxSec [(1, 2),(0, 7),(4, 6)] = (0, 7)
maxSec :: [(Int, Int)] -> (Int, Int)
maxSec xs = foldr maxL (0,0) xs

--Ej.maxL (1, 2) (0, 7) = (0, 7).
maxL :: (Int, Int) -> (Int, Int) -> (Int, Int)
maxL (x1, y1) (x2, y2)
    | abs (y1 - x1) >= abs (y2 - x2) = (x1, y1)
    | otherwise = (x2, y2)