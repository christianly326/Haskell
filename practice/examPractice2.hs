-- Christian
-- ExamPractice2

import Data.Char (toUpper)

-- 1. Basic Functions

-- Define a function square that takes an integer and returns its square.
square :: Int -> Int
square x = x * x
-- Create a function rectanglePerimeter that calculates the perimeter of a rectangle given its width and height.
rectanglePerimeter :: Int -> Int -> Int
rectanglePerimeter w h = result
    where result = (w * 2) + (h * 2)


-- 2. Recursion
-- Write a recursive function sumDigits that calculates the sum of all digits in a given non-negative integer.
-- Example: sumDigits 123 should return 6.

sumDigits :: Int -> Int
sumDigits x
    | x < 0 = error "Negative number given"
    | x == 0 = 0
    | x > 0 = (x `mod` 10) + sumDigits (x `div` 10) 
-- Define a recursive function gcdRec that finds the greatest common divisor of two integers using the Euclidean algorithm.


-- 3. List Manipulation
-- Write a function removeEvens that takes a list of integers and returns a list with all even numbers removed.
removeEvens :: [Int] -> [Int]
removeEvens [] = error "Empty list has been given"
removeEvens (x:xs)
    | (x `mod` 2) == 1 = x : removeEvens xs
    | otherwise = removeEvens xs


-- Define a function multiplyAll that multiplies all elements in a list of integers and returns the result.

multiplyAll :: [Int] -> Int
multiplyAll x = foldr (*) 1 x
-- 4. Pattern Matching and Guards
-- Write a function temperatureStatus that takes a temperature in Celsius and returns a string indicating "Cold" for temperatures below 10, "Warm" for temperatures between 10 and 25, and "Hot" for temperatures above 25.

temperatureStatus :: Int -> String
temperatureStatus x 
    | x < 10 = "Cold"
    | x < 26 = "Warm"
    | otherwise = "Hot"

-- Define a function describeAge that returns "Child" if age is less than 13, "Teen" if age is between 13 and 19, and "Adult" otherwise.

describeAge :: Int -> String
describeAge x 
    | x < 13 = "Child"
    | x < 20 = "Teen"
    | otherwise = "Adult"
-- 5. Higher-Order Functions
-- Define a function applyThrice that takes a function and an argument and applies the function to the argument three times in a row.
applyThrice :: (a -> a) -> a -> a
applyThrice fn x = fn (fn (fn x))
-- Write a function filterOdds that takes a list of integers and returns only the odd numbers, using the filter function.

filterOdds :: [Int] -> [Int]
filterOdds x = filter odd x 
-- 6. Map and Lambda Expressions
-- Define a function squareAll that takes a list of integers and returns a list where each integer is squared using map and a lambda expression.
squareAll :: [Int] -> [Int]
squareAll x = map (\x -> x * x) x

-- Write a function toUpperStrings that takes a list of strings and returns a list where each string is converted to uppercase.

toUpperStrings :: [String] -> [String]
toUpperStrings [] = []
toUpperStrings (x:xs) = map toUpper x : toUpperStrings xs 
-- 7. List Comprehensions
-- Create a function multiplesOfFive that returns a list of the first n multiples of 5.
-- multiplesOfFive :: Int -> [Int]
-- multiplesOfFive x 
--     | x < 0 = error "Negative number given"
--     | x == 0 = []
--     | otherwise = multiplesOfFive (x-1) ++ [x * 5]

multiplesOfFive :: Int -> [Int]
multiplesOfFive x = take x [n | n <- [2..], n `mod` 5 == 0]

-- Define a function firstNCubes that returns a list of the first n cubes of integers starting from 1.

firstNCubes :: Int -> [Int]
firstNCubes x 
    | x < 0 = error "Negative number given"
    | x == 0 = [] 
    | x > 0 = firstNCubes (x - 1) ++ [x * x * x]

-- 8. Prime Number Sequence
-- Write a function isPrime that checks if a given integer is a prime number.

isPrime :: Int -> Bool
isPrime x
    | x <= 1 = False
    | otherwise = null [n | n <- [2..isqrt x], x `mod` n == 0]
    where
        isqrt = floor . sqrt . fromIntegral

-- Use isPrime to define a function firstNPrimes that generates a list of the first n prime numbers.

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n [x | x <- [2..], isPrime x]
-- 9. Using Fold
-- Implement a function productList that calculates the product of a list of integers using foldl.

productList :: [Int] -> Int
productList x = foldl (*) 1 x
-- Write a function maxList that finds the maximum element in a list of integers using foldr.

maxList :: [Int] -> Int
maxList (x:xs) = foldr max x xs
-- 10. Custom Data Types
-- Define a custom data type Vehicle with three constructors: Car (with number of seats), Bike (with wheel count), and Truck (with maximum load capacity).
-- Write a function describeVehicle that takes a Vehicle and returns a description string based on the type and properties of the Vehicle.
-- 11. Recursive List Operations
-- Write a recursive function takeFirstN that takes an integer n and a list, and returns a list with the first n elements of the original list.
-- Define a function countOdds that takes a list of integers and returns the count of odd numbers in the list.
-- 12. Binary Search Tree
-- Define a binary search tree data structure in Haskell.
-- Write a function deleteBST to delete an integer from a binary search tree.
-- Implement a function inorderTraversal that returns a list of all elements in the binary search tree in ascending order.
-- Bonus Challenge
-- Write a function mergeSort to sort a list of integers using the merge sort algorithm.
-- Define a function balancedBST that constructs a balanced binary search tree from a sorted list of integers.
