




-----------------------------------------revisar----------------------------------------------



data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show, Eq)

headCL :: CList a -> a
--headCL EmptyCL = error "headCL is not defined for an empty list"
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
--tailCL EmptyCL = error "tailCL is not defined for an empty list"
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc _ ys b) = case ys of
    EmptyCL -> CUnit b
    _       -> Consnoc (headCL ys) (tailCL ys) b

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit x) = True
isCUnit _ = False


--b) Definir una funci´on reverseCL que toma una CList y devuelve su inversa.
reverseCL :: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = (CUnit x)
reverseCL (Consnoc i xs f) = Consnoc f (reverseCL xs) i

--c) Definir una funci´on inits que toma una CList y devuelve una CList con todos los posibles
--inicios de la CList.
inits :: CList a -> CList (CList a)
inits EmptyCL = CUnit EmptyCL
inits (CUnit x) = Consnoc EmptyCL EmptyCL (CUnit x)
inits (Consnoc i xs f) = Consnoc EmptyCL (mapCL (\ys -> Consnoc i ys f) (inits xs)) (CUnit (Consnoc i EmptyCL f))

mapCL :: (a -> b) -> CList a -> CList b
mapCL _ EmptyCL = EmptyCL
mapCL f (CUnit x) = CUnit (f x)
mapCL f (Consnoc i xs f') = Consnoc (f i) (mapCL f xs) (f f')

-- d) Definir una funci´on lasts que toma una CList y devuelve una CList con todas las posibles
-- terminaciones de la CList.
lasts :: CList a -> CList (CList a)
lasts EmptyCL = CUnit EmptyCL
lasts (CUnit x) = Consnoc (CUnit x) EmptyCL EmptyCL
lasts (Consnoc i xs f) = Consnoc (Consnoc i xs f) (mapCL (\ys -> Consnoc (headCL ys) (tailCL ys) f) (lasts xs)) EmptyCL


--e) Definir una funci´on concatCL que toma una CList de CList y devuelve la CList con todas ellas
--concatenadas