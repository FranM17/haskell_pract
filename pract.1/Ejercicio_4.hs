foo2 :: (y -> x) -> (z -> y) -> z -> x
foo2 x y z = x (y z)

foo3 :: (y -> z -> x) -> y -> z -> x
foo3 x y z = x y z

foo4 :: (y -> z) -> y -> [z] -> [z]
foo4 x y z = x y : z

foo5 :: x -> ([z] -> [x]) -> [z] -> [x]
foo5 x y z = x : y z

foo6 :: [x] -> ([z] -> [x]) -> [z] -> [x]
foo6 x y z = x ++ y z

foo7 :: [[a]] -> ([[a]] -> Bool) ->  [a]
foo7 a b = if b a then head a else [ ]

foo8 :: [a] -> ([a] -> Bool) -> [a]
foo8 a b = if b a then a else [ ]