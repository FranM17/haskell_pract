import Control.Concurrent.STM (check)
import Data.Binary.Get (label)

data BST a = Hoja | Nodo (BST a) a (BST a) deriving (Show, Eq)
--data BST a = Empty | Node (BST a) a (BST a) deriving (Show, Eq)

--maximun :: BST a -> a, que calcula el m´aximo valor en un bst.

maximum' :: BST a -> a
maximum' (Nodo l a Hoja ) = a
maximum' (Nodo l a r ) = maximum' r

--checkBST :: BST a -> Bool, que chequea si un `arbol BSTario es un bst.

checkBST :: Ord a => BST a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja a Hoja) = True
checkBST (Nodo Hoja a r) = minimum' r >= a && checkBST r
checkBST (Nodo l a Hoja) = maximum' l <= a && checkBST l
checkBST (Nodo l a r) = maximum' l <= a && a <= minimum' r && checkBST l && checkBST r

{-

checkBST (Nodo Hoja a r) = all (< a) (inOrder r) && checkBST r
checkBST (Nodo l a Hoja) = all (> a) (inOrder l) && checkBST l
checkBST (Nodo l a r) = all (> a) (inOrder l) && all (< a) (inOrder r) && checkBST l && checkBST r

-- Helper function to get the in-order traversal of the BST
inOrder :: BST a -> [a]
inOrder Hoja = []
inOrder (Nodo l a r) = inOrder l ++ [a] ++ inOrder r

-}

member :: Ord a => a -> BST a -> Bool
member a Hoja = False
member a (Nodo l b r )
    | a == b = True
    | a < b = member a l
    | a > b = member a r

minimum' :: BST a -> a
minimum' (Nodo Hoja a r ) = a
minimum' (Nodo l a r ) = minimum' l

insert :: Ord a => a -> BST a -> BST a
insert a Hoja = Nodo Hoja a Hoja
insert a (Nodo l b r)
    | a <= b = Nodo (insert a l) b r
    | otherwise = Nodo l b (insert a r )

delete :: Ord a => a -> BST a -> BST a
delete _ Hoja = Hoja
delete z (Nodo l b r)
    | z < b = Nodo (delete z l) b r
    | z > b = Nodo l b (delete z r)
    | otherwise = case r of
        Hoja -> l
        _    -> let y = minimum' r
                in Nodo l y (delete y r)

