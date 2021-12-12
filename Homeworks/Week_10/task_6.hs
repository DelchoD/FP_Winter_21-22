--Task 6
--Write a function that sums the unique numbers in the sublists of a list.

import Data.List
main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45 

sumNonUnique :: [Int] -> Int
sumNonUnique xs = helper (nub xs) 0
    where
        helper :: [Int]->Int->Int
        helper filteredList total
         |null filteredList = total
         |length (filter (\ x -> (head filteredList) == x) xs) >= 2 =helper (tail filteredList) (total + sum (filter (\ x -> (head filteredList) == x) xs))
         |otherwise = helper (tail filteredList) total

sumUniqueSingle :: [Int] -> Int
sumUniqueSingle xs = sum xs - sumNonUnique xs
    
sumUnique :: [[Int]] -> Int
sumUnique xss
    | null xss = 0
    | otherwise = sumUniqueSingle (head xss) + sumUnique (tail xss)
