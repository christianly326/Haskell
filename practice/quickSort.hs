-- Quick Sort algorithm

quickMergeBigger :: (Ord a) => a -> [a] -> [a]
quickMergeBigger pivot [] = []
quickMergeBigger pivot (x:xs)
    | x > pivot = x : quickMergeBigger pivot xs
    | otherwise = quickMergeBigger pivot xs

quickMergeSmaller :: (Ord a) => a -> [a] -> [a]
quickMergeSmaller pivot [] = []
quickMergeSmaller pivot (x:xs)
    | x <= pivot = x : quickMergeSmaller pivot xs
    | otherwise = quickMergeSmaller pivot xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = [] 
quickSort (x:xs) =
    let smallerSorted = quickMergeSmaller x xs
        biggerSorted  = quickMergeBigger x xs
    in quickSort smallerSorted ++ [x] ++ biggerSorted
