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

isAccending ::( Ord a)=> [a]->Bool
isAccending (x:y:[]) = if x <= y then True else False
isAccending (x:y:xs) = if x <= y then isAccending (y:xs) else False

isDecending ::( Ord a)=> [a]->Bool
isDecending (x:y:[]) = if x >= y then True else False
isDecending (x:y:xs) = if x >= y then isDecending (y:xs) else False

isSorted ::( Ord a)=> [a]->Bool
isSorted xs =  isAccending xs || isDecending xs

isSortedXs ::(Eq a, Ord a)=> [a]->Bool
isSortedXs xs= (sort xs == xs) || (reverse (sort xs) == xs)