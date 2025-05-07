data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp deriving (Show, Eq)

--Defina un evaluador eval :: Aexp → Int. ¿C´omo maneja los errores de divisi´on por 0?7

evall :: Aexp -> Int
evall (Num x) = x
evall (Prod x y) = evall x * evall y
evall (Div x y)
    | evall y /= 0 = div (evall x) (evall y)
    | otherwise = error "Error: no se puede dividir por 0"

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = do
    a <- seval x
    b <- seval y
    Just (a * b)
seval (Div x y) = do
    a <- seval x
    b <- seval y
    if b /= 0 then Just (div a b) else Nothing

