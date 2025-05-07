{-
    10. Definir una funci ́on fromList :: [ a ] -> Heap a, que cree un leftist heap a partir de una lista,
    convirtiendo cada elemento de la lista en un heap de un solo elemento y aplicando la funci ́on merge
    hasta obtener un solo heap. Aplicar la funci ́on merge [lg n] veces, donde n es la longitud de la
    lista que recibe como argumento la funci ́on, de manera que fromList sea de orden O(n).
    Pr ́actica 2 P ́agina 3
-}

--rank izq >= rank der 
--rank long de la espina derecha de cada nodo 
--espina derecha es ruta más corta a hoja 
--elementos de espina derecha ordenados 

type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a) deriving (Show, Eq)

fromList :: Ord a => [a] -> Heap a
fromList [] = E
fromList xs = foldr insert E xs

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (N 1 x E E) h

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h E = h
merge E h = h
merge h1@(N _ x l1 r1) h2@(N _ y l2 r2)
    | x <= y    = makeHeap x l1 (merge r1 h2)
    | otherwise = makeHeap y l2 (merge r2 h1)

makeHeap :: a -> Heap a -> Heap a -> Heap a
makeHeap x a b
    | rankk a >= rankk b = N (rankk b + 1) x a b
    | otherwise          = N (rankk a + 1) x b a

rankk :: Heap a -> Int
rankk E = 0
rankk (N r _ _ _) = r

{-
    go xs (N 0 (maximum xs) E E)
        where 
            go [] T = T
            go xs h1@(N r x l r) 
                | rankk l >= r = 
    fromList (x:xs) = let 
        pares = 
        impares = 
        der = 
        izq = 
        in N (calculate_rank) x der izq

    merge :: Ord a => Heap a -> Heap a -> Heap a

    insert :: Ord a => a -> Heap a -> Heap a
    findMin :: Ord a => Heap a -> a
    deleteMin :: Ord a => Heap a -> Heap a
-}