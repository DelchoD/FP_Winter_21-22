--Task 3
--Write TWO functions that remove every element equal to x in a list.
    --Implementation details:
    -- The first solution should use pattern matching (WITHOUT HEAD AND TAIL);
    -- The second - higher order functions and PARTIAL APPLICATION.

import Data.Char
main :: IO()
main = do

    print $ removeAll 5 [5] == []
    print $ removeAll 4 [4, 4] == []
    print $ removeAll 5 [1] == [1]
    print $ removeAll 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAll 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    print $ removeAllHOF 5 [5] == []
    print $ removeAllHOF 4 [4, 4] == []
    print $ removeAllHOF 5 [1] == [1]
    print $ removeAllHOF 5 [5, 1, 5, 3, 5] == [1, 3]
    print $ removeAllHOF 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
    
removeAll :: (Eq a) => a -> [a] -> [a]
removeAll _ [] = []
removeAll n (x:xs)
 |n/=x = x:removeAll n xs
 |otherwise = removeAll n xs

removeAllHOF :: (Eq a) => a -> [a] -> [a]
removeAllHOF n = filter (/= n)