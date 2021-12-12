--Task 6
--Define a predicate that checks whether a number is prime.
    --solve using guards;
    --solve using list comprehension in ONE line.

import Data.List
import Data.Char
main :: IO()
main = do
    print $ isPrimeG 1  == False
    print $ isPrimeG 2  == True
    print $ isPrimeG 3  == True
    print $ isPrimeG 6  == False
    print $ isPrimeG 61  == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True

isPrimeG :: Int -> Bool
isPrimeG x
 | x < 0 = error "x should be greater than zero"
 | x == 1 = False
 | otherwise = length (filter (\ n -> rem x n == 0) [2 .. x]) == 1

isPrimeLC :: Int -> Bool
isPrimeLC x = x /= 1 && length [ n | n <- [2 .. x], rem x n == 0] == 1