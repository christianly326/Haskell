isPalindrome :: String -> Bool

myTail :: [a] -> [a]
myTail [] = error "myTail: emptyList"
myTail [_] = []
myTail (_:t) = t


removeLast :: [a] -> [a]
removeLast [] = error "removeLast: Can't remove an emptylist"
removeLast [_] = []
removeLast (h:t) = h : removeLast(t)

myHead :: [a] -> a
myHead [] = error "MyHead: emptyList given"
myHead (h:_) = h

myLast :: [a] -> a
myLast [] = error "MyLast: emptyList given"
myLast [x] = x
myLast (_:xs) = myLast xs


isPalindrome r
    | length r <= 1 = True
    | myHead r /= myLast r = False
    | otherwise = isPalindrome(myTail (removeLast r))