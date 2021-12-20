--Task 1
--Write a function that checks whether an element is present in a list.
    --Implementation detail: You cannot use elem, head or tail!

import Data.Char
main :: IO()
main = do

    print $ isPresent 0 [0, -1, 2] == True
    print $ isPresent 1 [0, 1, 2] == True
    print $ isPresent 2 [0, 1, -2] == False
    print $ isPresent 3 [0, 1, 2] == False
    print $ isPresent 'a' ['b', 'z', 'a'] == True
    
isPresent :: (Eq a) => a -> [a] -> Bool
isPresent _ [] = False
isPresent n (x:xs) =  n==x || isPresent n xs