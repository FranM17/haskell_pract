type Color = (Int, Int, Int)

mezclar :: Color -> Color -> Color
mezclar (r1, g1, b1) (r2, g2, b2) = 
    ((r1 + r2) `div` 2, (g1 + g2) `div` 2, (b1 + b2) `div` 2)