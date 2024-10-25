-- CA320 Computability and Complexity
--
-- <Christian Michael Lagura Yacapin>
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = 
    let s = (a + b + c) / 2
    in sqrt (s * (s - a) * (s - b) * (s - c))

isSum :: Integer -> Integer -> Integer -> Bool
isSum x y z = x == y + z || z == y + x || y == x + z

isSumFloat :: Float -> Float -> Float -> Bool
isSumFloat x y z = x == y + z || z == y + x || y == x + z
triangleAreaValid :: Float -> Float -> Float -> Float
triangleAreaValid a b c
    | isSumFloat a b c = triangleArea a b c 
    | otherwise = error "Not a Triangle!"
