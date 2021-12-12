--Task 5
--Define a predicate that checks whether the digits of a non-negative whole number are ordered in an ascending order.

--as from the test cases, we do not need the type be again Int so this is one possible solution :)
import Data.List
import Data.Char
main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
    --print $ hasIncDigits (-121)

hasIncDigits :: Int -> Bool
hasIncDigits x
 | x < 0 = error "x should be greater than zero"
 | otherwise = show x == (sort $ show x)