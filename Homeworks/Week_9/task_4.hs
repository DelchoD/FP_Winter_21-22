--Task 4
--Define a predicate that checks whether two numbers are amicable.

import Data.Char
main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

sumDiv :: Int->Int
sumDiv x = sum $ filter (\ n -> mod x n == 0) [1 .. x-1]

areAmicable :: Int -> Int -> Bool
areAmicable x y =  x == sumDiv y || sumDiv x == y

