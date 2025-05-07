data Linea = Posc Int [Char] deriving (Show, Eq)

vacia :: Linea
vacia = Posc 0 []

moverIzq :: Linea -> Linea
moverIzq (Posc p xs)
    | p > 0 = Posc (p - 1) xs
    | otherwise = Posc p xs

moverDer :: Linea -> Linea
moverDer (Posc p xs)
    | p < length xs = Posc (p + 1) xs
    | otherwise = Posc p xs

moverIni :: Linea -> Linea
moverIni (Posc _ xs) = Posc 0 xs

moverFin :: Linea -> Linea
moverFin (Posc _ xs) = Posc (length xs) xs

insertar :: Char -> Linea -> Linea
insertar c (Posc p xs) = Posc (p + 1) (take p xs ++ [c] ++ drop p xs)

borrar :: Linea -> Linea
borrar (Posc 0 xs) = Posc 0 xs
borrar (Posc p xs) = Posc (p - 1) (take (p - 1) xs ++ drop p xs)



{-

    go c posc 0 xs []
        where
            go c posc cont xs list
                | posc == cont = (Posc (posc+1) Cad (list ++ [c] ++ xs))
                | otherwise = go c posc (cont + 1) (tail xs) (list ++ [head xs])
-}
--            (head xs) : go c posc (cont + 1) (tail xs) 
