--Task 9
--Define a function that returns the product of all natural numbers in a list, the sum of the divisors of which is a multiple of k.
    --Implementation detail: Use list comprehension!

import Data.List
main :: IO()
main = do

    print $ naturalProduct [-1, 0, -2, -3] 5  == 0 -- There are no natural numbers
    print $ naturalProduct [5, 10] 5  == 0 -- Sum of the divisors for 5 is 1 and for 10 is 1+2+5=8
    print $ naturalProduct [95, 75, 15, 55, 11, 14, 18, 35, 25] 5  == 1330


getDivisors :: Int-> [Int]
getDivisors num
 | num < 0 = [1]
 |otherwise = [ x | x <- [1 .. num-1], mod num x == 0]

naturalProductWrapper :: [Int] -> Int -> Int
naturalProductWrapper [] _ = 1
naturalProductWrapper (x:xs) num
 | (sum (getDivisors x)) `mod` num == 0 = x * (naturalProductWrapper xs num)
 | otherwise = (naturalProductWrapper xs num)

naturalProduct :: [Int] -> Int -> Int
naturalProduct xs num
 | (naturalProductWrapper xs num) <=1 = 0
 | otherwise = naturalProductWrapper xs num