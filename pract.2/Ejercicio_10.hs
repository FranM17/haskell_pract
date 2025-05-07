{-
10. Definir una funci ́on fromList :: [ a ] → Heap a, que cree un leftist heap a partir de una lista,
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
data Heap a = E | N Rank a (Heap a) (Heap a)

fromList :: [a] -> Heap a
fromList [] = E
fromList [x] = N 0 x E E
fromList xs = let 
    min = minimum xs
    l = length xs
    izq = if mod l 2 then take l xs else take (l+1) xs
    der = drop xs
    in N calculate_rank x (fromList izq) (fromList der)

fromList (x:xs) = let 
    pares =
    impares =
    der = 
    izq = 
    in N (calculate_rank) x der izq

merge :: Ord a ⇒ Heap a → Heap a → Heap a

insert :: Ord a ⇒ a → Heap a → Heap a
findMin :: Ord a ⇒ Heap a → a
deleteMin :: Ord a ⇒ Heap a → Heap a
