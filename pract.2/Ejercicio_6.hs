data Tree a = Hoja | Nodo (Tree a) a (Tree a) deriving (Show, Eq)

completo :: a -> Int -> Tree a
completo _ 0 = Hoja
completo x d = Nodo (completo x (d-1)) x (completo x (d-1))
--al que dado un valor x de tipo a y un entero d, crea un ´arbol
--binario completo de altura d con el valor x en cada nodo.

alturaT :: Tree a -> Int
alturaT Hoja = 0
alturaT (Nodo l x r) = 1 + max (alturaT l) (alturaT r)

balanceado :: a -> Int -> Tree a
balanceado _ 0 = Hoja
balanceado x n 
    | odd n = Nodo (balanceado x (div n 2)) x (balanceado x (div n 2))
    | otherwise = Nodo (balanceado x (div n 2)) x (balanceado x (div n 2-1))



--tal que dado un valor x de tipo a y un entero n, crea un ´arbol
--binario balanceado de tama˜no n, con el valor x en cada nodo