-- Haskell code to reverse a list
-- Christian Michael Lagura Yacapin

myLast :: [a] -> a
myLast [] = error "myLast: Empty list has been given"
myLast [x] = x
myLast (_:t) = myLast t

removeLast :: [a] -> [a]
removeLast [] = error "removeLast: Empty List has been given"
removeLast [_] = []
removeLast (h:t) = h : removeLast t

reverseList :: [a] -> [a]
reverseList [] = error "reverseList: Empty List has been given"
reverseList [x] = [x]
reverseList (h:t) = myLast(h:t) : reverseList(removeLast (h:t))

