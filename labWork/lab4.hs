-- CSC1048 Computability and Complexity
--
-- <Christian Michael Lagura Yacapin>

-- 1. Develop Your own List functions

myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x:xs) y = x : myAppend xs y

myHead :: [a] -> a
myHead [] = error "myHead empty List given"
myHead (x:_) = x

myLast :: [a] -> a
myLast [] = error "empty list given"
myLast [x] = x
myLast (_:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "empty list given"
myTail (_:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "Empty list given"
myInit [_] = []
myInit (x:xs) = x : myInit xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse x = myLast x : myReverse (myInit x)

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat [x] = x
myConcat (x:xs) = myAppend x (myConcat xs)

mySum :: Num a => [a] -> a
mySum [] = 0
mySum [x] = x
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct [] = error "Empty list given"
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "Empty List given"
myMaximum [x] = x
myMaximum (x : xs) = 
    let maxTail = myMaximum xs
    in if x > maxTail then x else maxTail

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Empty list given"
myMinimum [x] = x
myMinimum (x:xs) = if x < myMinimum xs then x else myMinimum xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) =
    if x == y
    then True
    else myElem x ys

myDelete :: Eq a => a -> [a] -> [a]
myDelete _ [] = []
myDelete x (y:ys) =
    if x == y
    then ys
    else y : myDelete x ys

myUnion :: Eq a => [a] -> [a] -> [a]
myUnion x [] = x
myUnion x (y:ys) =
    if myElem y x
    then myUnion x ys
    else myUnion (myAppend x [y]) ys

myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect _ [] = []
myIntersect x (y : ys) =
    if myElem y x
    then y : myIntersect x ys
    else myIntersect x ys
