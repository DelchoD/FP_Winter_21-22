--Task 4
--Define a function that returns a list of the prime numbers in a given interval.

    --Implementation detail:

    --Use list comprehension wherever possible!

import Data.List
main :: IO()
main = do
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97] 
  
isPrime :: Int -> Bool
isPrime n = n > 1 && (null $ filter (\ d -> mod n d == 0) [2 .. n - 1])  

primesInRange :: Int -> Int -> [Int]
primesInRange start final = [ x | x <- [(min start final) .. (max start final)], isPrime x] 
