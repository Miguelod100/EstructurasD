distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x1,y1) (x2,y2) = ((x2 - x1)**2 + (y2 - y1)**2)**(1/2)

hipotenusa :: Float -> Float -> Float
hipotenusa b h = (b**2 + h**2)**(1/2)

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = ( (-b + (b**2 - 4*a*c)**(1/2)) / 2*a, (-b - (b**2 - 4*a*c)**(1/2)) / 2*a )

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo x y z = let s = ((x+y+z)/2)
                      in (s*(s-x)*(s-y)*(s-z))**(1/2)

comparador :: Int -> Int -> Int
comparador x y = if x < y then -1
                 else if x > y then 1
                 else 0



maximo :: Int -> Int -> Int -> Int
maximo x y z = if x > y && x > z then x
               else if y > x && y > z then y
               else z

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w = if x <= y || z <= w then False
                        else if y <= z then False
                        else True 