--Task 5
--Define a function that takes a whole number and returns its ascending left suffix. The ascending left 
--suffix of a number is the number that forms a strictly ascending sequence, if read from right to left.

import Data.List
main :: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6 

reverseOrdSuff :: Integer -> Integer
reverseOrdSuff number = helper number 0
    where
        helper :: Integer -> Integer -> Integer
        helper leftOver suffix 
         |(mod leftOver 10) < (mod (div leftOver 10) 10) = helper (div leftOver 10) (suffix*10 + (mod leftOver 10)) 
         |otherwise = suffix*10 + (mod leftOver 10)
