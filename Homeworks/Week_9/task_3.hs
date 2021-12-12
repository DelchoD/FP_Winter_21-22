--Task 3
--Define a predicate that checks whether a non-negative number is a palindrome.

--as from the test cases, we do not need the type be again Int so this is one possible solution :)
import Data.Char
main :: IO()
main = do
    print $ isPalindrome 6  == True
    print $ isPalindrome 1010  == False
    print $ isPalindrome 505  == True
    print $ isPalindrome 123321  == True
    print $ isPalindrome 654  == False

isPalindrome :: Int->Bool
isPalindrome x = show x == (reverse $ show x)
