-- CSC1048 Computability and Complexity
--
-- <Christian Michael Lagura Yacapin>

-- 1
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x : xs) =
    if x == last xs
    then isPalindrome (take (length xs - 1) xs)
    else False

-- 2. Shortest list
shortest :: [[a]] -> [a]
shortest [] = error "empty list given"
shortest [x] = x
shortest (x:xs) =
    let restShortest = shortest xs
    in if length x < length restShortest
        then x
        else restShortest
-- 3
type Poly = [Int]
sumPoly :: Poly -> Poly -> Poly
sumPoly [] b = b
sumPoly a [] = a
sumPoly (x:xs) (y:ys) = x + y : sumPoly xs ys

-- 4
evalPoly :: Int -> Poly -> Int
evalPoly _ [] = 0
evalPoly x (a:as) =  a + x * evalPoly x as