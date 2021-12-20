--Task 8
--Define a function rf that takes two unary, whole-number functions as parameters
--- f and g and returns a binary function that takes a list - xs as its first argument,
--  and an unary function - h as its second argument. The result from the call to rf should
--  be a list containing elements of the form h(x) where x spans xs and f(x) > g(x).

import Data.List
main :: IO()
main = do

    print $ (rf ((-) (-4)) (* (-2))) [1..10] (* 3) == [15,18,21,24,27,30] -- only 5, 6, 7, 8, 9 and 10 satisfy the condition

rf :: (Int -> Int) -> (Int -> Int) -> ([Int] -> (Int -> Int) -> [Int])
rf f g = (\ xs h -> [ h x | x <- xs, f x > g x])
