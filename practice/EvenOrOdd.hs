-- Code that returns True for Even False for Odd

isEven :: Int -> Bool

isEven 0 = True
isEven 1 = False
isEven n = isEven(n-2)
