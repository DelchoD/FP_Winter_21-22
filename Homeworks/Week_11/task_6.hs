--Task 6
--Define a function that returns a tuple (b, n) where b is True if for every element in two lists - xs, ys: yi = n + xi.

import Data.Char
main :: IO()
main = do

    print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
    print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 6]  == (False, 0)
    print $ isImage [1, 2] [-1, -2]  == (False, 0)
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 4]  == (False, 0)

checkImage :: (Num a, Eq a)=> [a]->[a]->a->Bool
checkImage [] _ _ = True
checkImage _ [] _ = True
checkImage (x:xs) (y:ys) div
 |length xs /= length ys = False
 |y - x == div = checkImage xs ys div
 |otherwise = False

isImage ::(Num a, Eq a)=> [a]-> [a]-> (Bool, a)
isImage xs ys 
 |checkImage xs ys (head ys - head xs) == False = (checkImage xs ys (head ys - head xs), 0)
 |otherwise = (checkImage xs ys (head ys - head xs), (head ys - head xs))


