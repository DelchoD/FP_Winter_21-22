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
getDivisors num = if num< 0 then [1] else [ x | x <- [1 .. num-1], mod num x == 0]

naturalProductWrapper :: [Int] -> Int -> Int
naturalProductWrapper [] _ = 1
naturalProductWrapper (x:xs) num = if (sum (getDivisors x)) `mod` num == 0 then x * (naturalProductWrapper xs num) else (naturalProductWrapper xs num) 

naturalProduct :: [Int] -> Int -> Int
naturalProduct xs num = if (naturalProductWrapper xs num) <=1 then 0 else naturalProductWrapper xs num