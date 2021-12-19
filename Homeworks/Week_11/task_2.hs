--Task 2
--Write a function that removes the first element in a list that is equal to x by going from left to right.
    --Implementation detail: You cannot use head or tail!

import Data.Char
main :: IO()
main = do

    print $ removeFirst 5 [5, 1, 5, 3, 5]  == [1, 5, 3, 5]
    print $ removeFirst 3 [5, 1, 5, 3, 5]  == [5, 1, 5, 5]
    
removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst n xs = helper 0 n xs
    where
     helper :: (Eq a) => Int -> a -> [a] -> [a]
     helper _ _ [] = []
     helper 0 n (x:xs)= if x == n then helper 1 n xs else x:helper 0 n xs
     helper 1 n (x:xs)= x:helper 1 n xs
     --helper 2 n (x:xs)= x:helper 2 n xs