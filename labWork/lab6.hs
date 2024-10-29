-- CSC1048 Computability and Complexity
--
-- <Christian Michael Lagura Yacapin>

data AVLTree a = Empty | Root a (AVLTree a) (AVLTree a) Int
    deriving(Show, Eq, Ord)


height :: AVLTree a -> Int
height Empty = 0
height (Root _ _ _ h) = h

balanceFactor :: AVLTree a -> Int
balanceFactor Empty = 0
balanceFactor (Root _ left right _) = height right - height left