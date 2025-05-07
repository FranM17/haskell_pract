cambios :: Eq a => [a] -> [Int]
cambios xs = [i | (i, (x, y)) <- zip [0..] (zip xs (tail xs)), x /= y]

oblongoNumber :: [Int]
oblongoNumber = [x * y | (x, y) <- zip [0..10] [1..11]]

abundantes :: [Integer]
abundantes = [n | n <- [1..], let sumaDiv = sum [d | d <- [1..(n `div` 2)], n `mod` d == 0], sumaDiv > n]

euler :: Int -> Int
euler n = sum [x | x <- [1..(n-1)], x `mod` 3 == 0 || x `mod` 5 == 0]

expandir :: [Int] -> [Int]
expandir [] = []
expandir xs = [y | x <- xs, y <- replicate x x]