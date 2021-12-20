--Task 7
--Define TWO functions - isSorted and isSortedXs that check whether a list is sorted in ascending or descending order.
    --isSorted should be implemented using pattern matching!
    --for isSortedXs you may use any built-in function.

import Data.List
main :: IO()
main = do

    print $ isSorted [-5, -5, -6]  == True
    print $ isSorted [-5, -5, -4] == True
    print $ isSorted [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSorted [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSorted [-100, -99, -99, -99] == True
    print $ isSorted [-100, -99, -99, -99, 100] == True
    print $ isSorted [100, 101, -102]   == False
    print $ isSorted [1, 2, 3, 4, 5, 6] == True
    print $ isSorted [-1, -2, -3, -4, -5, -6] == True
    --print $ isSorted [ ] == True
    print $ isSorted [1] == True

    print $ isSortedXs [-5, -5, -6]  == True
    print $ isSortedXs [-5, -5, -4] == True
    print $ isSortedXs [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
    print $ isSortedXs [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
    print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
    print $ isSortedXs [-100, -99, -99, -99] == True
    print $ isSortedXs [-100, -99, -99, -99, 100] == True
    print $ isSortedXs [100, 101, -102] == False
    print $ isSortedXs [1, 2, 3, 4, 5, 6] == True
    print $ isSortedXs [-1, -2, -3, -4, -5, -6] == True
    print $ isSortedXs [ ] == True
    print $ isSortedXs [1] == True

isAccending ::( Ord a, Num a)=> [a]->Bool
isAccending [x]= True
isAccending [x,y] = x <= y 
isAccending (x:y:xs)
 | x <= y = isAccending (y:xs)
 |otherwise = x <= y --instead of False

isDecending ::( Ord a, Num a)=> [a]->Bool
isDecending [x]= True
isDecending [x,y] = x >= y
isDecending (x:y:xs)
 | x>=y = isDecending (y:xs)
 | otherwise = x>=y

isSorted ::( Ord a, Num a)=> [a]->Bool
isSorted xs =  isAccending xs || isDecending xs

isSortedXs ::(Eq a, Ord a, Num a)=> [a]->Bool
isSortedXs xs= (sort xs == xs) || (reverse (sort xs) == xs)