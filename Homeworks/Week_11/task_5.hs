--Task 5
--Define a function that creates the tuple (x, y) where x is the smaller element of every 
--tuple and y - the larger. Make sure the list contains at least one element.
    --Implementation detail: You cannot use head or tail!

import Data.Char
main :: IO()
main = do

    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

type Point2D a = (a, a)

combine ::(Num a, Ord a) => [Point2D a]-> (a, a)
combine xs = helper xs 0 0
    where 
     helper ::(Num a, Ord a) => [Point2D a] -> a -> a -> (a, a)
     helper [] sumMin sumMax = (sumMin, sumMax)
     helper ((first, second):xs) sumMin sumMax
      | first <= second = helper xs (sumMin*10 + first) (sumMax*10 + second)
      | otherwise = helper xs (sumMin*10 + second) (sumMax*10 + first)