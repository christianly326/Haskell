-- CSC1048 Computability and Complexity
--
-- <Christian Michael Lagura Yacapin>



data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                deriving (Eq, Ord, Show)

leaf x = Root x Empty Empty 
addNode :: Ord a => a -> BinTree a -> BinTree a
addNode a Empty = leaf a
addNode a (Root v left right) =
    if a < v
    then Root v (addNode a left) right
    else Root v left (addNode a right)

makeTree :: Ord a => [a] -> BinTree a
makeTree [] = Empty
makeTree (x:xs) = addNode x (makeTree xs)


mpSort :: Ord (a) => [a] -> [a]
mpSort x = inOrder (makeTree x)


inOrder :: BinTree a -> [a]
inOrder Empty = []
inOrder (Root v left right) = inOrder left ++ [v] ++ inOrder right

hoAddNode :: Ord t => (t -> t -> Bool) -> t -> BinTree t -> BinTree t
hoAddNode _ a Empty = Root a Empty Empty 
hoAddNode fn x (Root a left right)   
    | fn x a  = Root a (hoAddNode fn x left) right  
    | otherwise  = Root a left (hoAddNode fn x right)



hoMakeTree :: Ord t => (t -> t -> Bool) -> [t] -> BinTree t
hoMakeTree _ [] = Empty
hoMakeTree fn (x:xs) = hoAddNode fn x (hoMakeTree fn xs)


hosort :: Ord t => (t -> t -> Bool) -> [t] -> [t]
hosort fn x = inOrder (hoMakeTree fn x)