--Task 11
--Define a function subLists :: [a] -> Int -> [[a]] that accepts a list n
-- and a natural number k and splits the elements of n into groups of length k.

import Data.List
main :: IO()
main = do
 
    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2  == [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 4  == [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10]]

makeList :: [a]-> Int ->[a]
makeList source number = take number source

removeFront :: [a] -> Int -> Int ->[a]
removeFront xs start final
 |start==final= xs
 |otherwise = removeFront (tail xs) (start+1) final

subLists :: [a] -> Int -> [[a]]
subLists xs counter = helper xs (length xs - counter -2)
    where 
        helper :: [a] -> Int -> [[a]]
        helper leftOver numberLeft
         |null leftOver = []
         |numberLeft <= 0 = makeList leftOver counter : helper [] 0
         |otherwise = makeList leftOver counter : helper (removeFront leftOver 0 counter) (length leftOver - counter-2)

