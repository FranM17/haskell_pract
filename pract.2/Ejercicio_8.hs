import System.Win32 (COORD(xPos))
--Definir una funci ́on fromOrdList :: [ a ] → RBT a, que cree un red black tree a partir de una
--lista ordenada sin elementos repetidos. La funci ́on debe ser de orden O(n)
data Color = R | B deriving (Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving (Show, Eq)
{-
fromOrdList :: [a] -> RBT a
fromOrdList [] = E
fromOrdList [x] = T B E x E
fromOrdList xs
    | mod (logBase 2 (length xs)) 2 = 

createRBT :: [a] -> RBT a
createRBT [] = E
createRBT [x] = T B E x E


createRBT xs = go xs E 
    where
        go [] t = t
        go [x] = 
        go (x:y:xs) (T color a1 v a2) 
            | color == B = T color (go (take )) 
-}

--logBase 10 100 
fromOrdList :: [a] -> RBT a
fromOrdList xs = fst $ buildTree xs (length xs)
    where
        buildTree [] _ = (E, [])
        buildTree xs n
            | n == 0 = (E, xs)
            | otherwise =
                    let leftSize = (n - 1) `div` 2
                        rightSize = n - 1 - leftSize
                        (leftTree, (x:rightRest)) = buildTree xs leftSize
                        (rightTree, rest) = buildTree rightRest rightSize
                    in (T B leftTree x rightTree, rest)

balanceColors :: RBT a -> RBT a
balanceColors E = E
balanceColors (T _ left x right) = makeBlack $ balance (T R (balanceColors left) x (balanceColors right))
    where
        makeBlack (T _ l v r) = T B l v r

balance :: RBT a -> RBT a
balance (T B (T R (T R a x b) y c) z d) = T R (T B a x b) y (T B c z d)
balance (T B (T R a x (T R b y c)) z d) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R (T R b y c) z d)) = T R (T B a x b) y (T B c z d)
balance (T B a x (T R b y (T R c z d))) = T R (T B a x b) y (T B c z d)
balance tree = tree
