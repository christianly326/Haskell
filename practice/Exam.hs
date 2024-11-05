
-- ### Question 1: Basic Syntax and Functions (10 points)

-- Write a function `squareList` that takes a list of integers and returns a new list with each integer squared.

-- ```haskell
-- squareList :: [Int] -> [Int]
-- ```

-- **Example:**

-- ```haskell
-- squareList [1, 2, 3, 4] == [1, 4, 9, 16]
-- squareList [] == []
-- ```

-- squareList :: [Int] -> [Int]
-- squareList [] = []
-- squareList x = map (\x -> x * x) x 



squareList :: [Int] -> [Int]
squareList [] = []
squareList (x : xs) = square x : squareList xs
  where square x = x * x

isEven :: Int -> Bool
isEven x 
  | (x `mod` 2) == 0 = True
  | otherwise = False

-- ---

-- ### Question 2: Recursion (15 points)

-- Define a recursive function `sumOfSquares` that takes an integer `n` and returns the sum of squares of all integers from 1 to `n`.

-- ```haskell
-- sumOfSquares :: Int -> Int
-- ```

-- **Example:**

-- ```haskell
-- sumOfSquares 3 == 14  -- 1^2 + 2^2 + 3^2 = 14
-- sumOfSquares 5 == 55
-- ```

-- **Note:** Assume `n` is always a non-negative integer.


sumOfSquares :: Int -> Int
sumOfSquares x
  | x < 0 = error "Negative number given"
  | x == 0 = 0
  | otherwise = (x * x) + sumOfSquares (x - 1)

-- ---

-- ### Question 3: Higher-Order Functions (20 points)

-- Write a function `applyTwice` that takes a function and a value, and applies the function to the value twice.

-- ```haskell
-- applyTwice :: (a -> a) -> a -> a
-- ```

-- **Example:**

-- ```haskell
-- applyTwice (+3) 10 == 16  -- ((10 + 3) + 3)
-- applyTwice (*2) 5 == 20   -- ((5 * 2) * 2)
-- ```

applyTwice :: (a -> a) -> a -> a
applyTwice fn x = fn (fn x)

-- ---

-- ### Question 4: List Comprehensions and Filtering (15 points)

-- Write a function `evensOnly` that takes a list of integers and returns only the even numbers from that list.

-- ```haskell
-- evensOnly :: [Int] -> [Int]
-- ```

-- **Example:**

-- ```haskell
-- evensOnly [1, 2, 3, 4, 5, 6] == [2, 4, 6]
-- evensOnly [1, 3, 5] == []
-- ```


evensOnly :: [Int] -> [Int]
evensOnly x = filter even x

-- ---

-- ### Question 5: Custom Data Types (20 points)

-- Define a data type `Shape` that represents geometric shapes. It should have three constructors:

-- - `Circle`, which takes a `Double` for the radius.
-- - `Rectangle`, which takes two `Double`s for the width and height.
-- - `Square`, which takes a single `Double` for the side length.

-- Then, write a function `area` that takes a `Shape` and calculates its area.

-- ```haskell
-- data Shape = Circle Double | Rectangle Double Double | Square Double
-- area :: Shape -> Double
-- ```

-- **Example:**

-- ```haskell
-- area (Circle 5) == 78.54       -- Approximate area of a circle with radius 5
-- area (Rectangle 4 5) == 20.0   -- Area of a rectangle with width 4 and height 5
-- area (Square 3) == 9.0         -- Area of a square with side length 3
-- ```

data Shape = Circle Double | Rectangle Double Double | Square Double

area :: Shape -> Double
area (Circle x) = radiusSquared * pi 
  where radiusSquared = x * x
area (Rectangle l h) = result
  where result = l * h
area (Square x) = x * x

-- ---

-- ### Question 6: Pattern Matching (10 points)

-- Write a function `describeList` that returns a string description of a list:

-- - "Empty list" if the list is empty.
-- - "Single item" if the list has one element.
-- - "Multiple items" if the list has more than one element.

-- ```haskell
-- describeList :: [a] -> String
-- ```

-- **Example:**

-- ```haskell
-- describeList [] == "Empty list"
-- describeList [42] == "Single item"
-- describeList [1, 2, 3] == "Multiple items"
-- ```

-- ---

describeList :: [a] -> String
describeList [] = "Empty list"
describeList [x] = "Single item"
describeList (x:xs) = "Multiple Items"


-- ### Question 7: Type Classes and Instances (20 points)

-- Define a custom data type `TrafficLight` with three constructors: `Red`, `Yellow`, and `Green`. Then, make `TrafficLight` an instance of the `Eq` and `Show` type classes, where:

-- - Two `TrafficLight` values are equal if they are the same color.
-- - `show` should return "Red Light", "Yellow Light", or "Green Light" for each respective constructor.

-- ```haskell
-- data TrafficLight = Red | Yellow | Green
-- instance Eq TrafficLight where
--   -- Implement equality checks here

-- instance Show TrafficLight where
--   -- Implement string representation here
-- ```

-- **Example:**

-- ```haskell
-- Red == Red == True
-- Red == Green == False
-- show Red == "Red Light"
-- show Green == "Green Light"
-- ```

-- ---

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

-- ### Question 8: List Manipulation (10 points)

-- Write a function `alternateSum` that takes a list of integers and returns the sum of elements at even indices minus the sum of elements at odd indices.

-- ```haskell
-- alternateSum :: [Int] -> Int
-- ```

-- **Example:**

-- ```haskell
-- alternateSum [1, 2, 3, 4, 5] == 3  -- (1 + 3 + 5) - (2 + 4)
-- alternateSum [10, 20, 30, 40] == -20  -- (10 + 30) - (20 + 40)
-- ```

-- ---

-- **End of Exam**

-- ---

-- This exam covers fundamental Haskell concepts such as basic functions, recursion, higher-order functions, custom data types, pattern matching, type classes, and list manipulation. Each question is weighted based on complexity, allowing for a mix of straightforward and challenging problems within the allotted time.