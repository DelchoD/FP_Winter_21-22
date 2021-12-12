--Task 8
--Define a function that accepts a string and removes all duplicate letters
    --Note: 
    -- Two characters are duplicate, if:
    --     - they represent the same character;
    --     - they are next to each other;
    --     - the first is upper-case and the second - lower-case (or vice versa).
import Data.List
import Data.Char
main :: IO()
main = do

    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    --                                                            ^^                 ^^                   ^^

checkDoubles :: Char -> Char -> Bool
checkDoubles left right = toLower left == toLower right && ((isLower left && isUpper right) || (isUpper left && isLower right))

reduceStrOnce :: [Char] -> [Char]
reduceStrOnce xs = helper xs
    where
        helper :: [Char] -> [Char]
        helper leftOver 
         |length leftOver == 1 = (leftOver!!0):[]
         |checkDoubles (leftOver!!0) (leftOver!!1) = leftOver!!2 : helper (tail(tail(tail leftOver)))
         |otherwise = (leftOver!!0) : helper (tail leftOver) 

reduceStr :: [Char] -> [Char]
reduceStr xs = helper xs (reduceStrOnce xs)
    where 
        helper :: [Char]->[Char]->[Char]
        helper start final
         |length start == length (reduceStrOnce final) = final
         |otherwise = helper final (reduceStrOnce final)
    