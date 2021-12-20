--Task 11
--Define a function subLists :: [a] -> Int -> [[a]] that accepts a list n
-- and a natural number k and splits the elements of n into groups of length k.

import Data.List
main :: IO()
main = do
 
    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2  == [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 4  == [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10]]

subLists :: [a] -> Int -> [[a]]
subLists [] _ = []
subLists xs counter = take counter xs : subLists (drop counter xs) counter


