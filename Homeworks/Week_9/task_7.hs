--Task 7
--Define a predicate that checks whether a number is perfect.
    --Implementation detail: Use list comprehension.
    --Note: A number is perfect if and only if it is natural and equal to the sum of its divisors excluding the number itself.

import Data.List
import Data.Char
main :: IO()
main = do
    print $ isPerfect 1  == False
    print $ isPerfect 6  == True
    print $ isPerfect 495   == False
    print $ isPerfect 33550336  == True

isPrime :: Int -> Bool
isPrime x = x /= 1 && length [ n | n <- [2 .. x], mod x n == 0] == 1

isPerfect :: Int -> Bool
isPerfect x = x > 0 && sum [ n | n <- [1 .. x], mod x n == 0] - x == x
