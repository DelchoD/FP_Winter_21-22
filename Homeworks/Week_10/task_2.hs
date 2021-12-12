--Task 2
--Define a function sumSpecialPrimes n d that returns the sum of the first n prime numbers that contain a digit d.

import Data.Char
main :: IO()
main = do

   print $ sumSpecialPrimes 5 2 == 392
   print $ sumSpecialPrimes 5 3 == 107
   print $ sumSpecialPrimes 10 3 == 462
   print $ hasDigit 5 523
    
isPrime :: Integer -> Bool
isPrime n = n > 1 && (null $ filter (\ d -> mod n d == 0) [2 .. n - 1])

hasDigit :: Int -> Integer -> Bool
hasDigit d number = any (\ x -> digitToInt x == d)  (show number)

sumSpecialPrimes :: Integer -> Int -> Integer
sumSpecialPrimes n d = helper 0 2 0
    where
        helper :: Integer -> Integer -> Integer -> Integer
        helper iter num sum
         |iter == n = sum
         |isPrime num && hasDigit d num = helper (iter +1) (num +1) (sum +num)
         |otherwise = helper iter (num +1) sum
