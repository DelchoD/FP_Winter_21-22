--Task 3
--Define TWO functions - mergeLinearRec and mergeXs. They accept two sorted lists and combine them into one that is also sorted.

    --Implementation details:

    --mergeLinearRec should implement a linear recursive process!
    --mergeXs should have only list manipulations!
    --mergeXs should be solved with ONE line of code!

import Data.List
main :: IO()
main = do

   print $ mergeLinearRec [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 4, 5, 6]
   print $ mergeLinearRec [1, 2, 3] [2] == [1, 2, 3]
   print $ mergeLinearRec [1, 2, 3] [7, 8, 9] == [1, 2, 3, 7, 8, 9]
   print $ mergeLinearRec [2, 3, 4, 5, 6] [1, 2, 3] == [1,2,3,4,5,6]
   print $ mergeLinearRec [2] [1, 2, 3] == [1,2,3]
   print $ mergeLinearRec [7, 8, 9] [1, 2, 3] == [1,2,3,7,8,9]
   print $ mergeLinearRec [7, 9, 11] [8, 10, 12] == [7,8,9,10,11,12]

   print $ mergeXs [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 4, 5, 6]
   print $ mergeXs [1, 2, 3] [2] == [1, 2, 3]
   print $ mergeXs [1, 2, 3] [7, 8, 9] == [1, 2, 3, 7, 8, 9]
   print $ mergeXs [2, 3, 4, 5, 6] [1, 2, 3] == [1,2,3,4,5,6]
   print $ mergeXs [2] [1, 2, 3] == [1,2,3]
   print $ mergeXs [7, 8, 9] [1, 2, 3] == [1,2,3,7,8,9]
   print $ mergeXs [7, 9, 11] [8, 10, 12] == [7,8,9,10,11,12]
    

mergeLinearRec :: [Int] -> [Int] -> [Int]
mergeLinearRec xs ys
    |null xs && null ys = []
    |null xs = head ys : mergeLinearRec [] (tail ys)
    |null ys = head xs : mergeLinearRec (tail xs) []
    |head xs < head ys = head xs : mergeLinearRec (tail xs) ys
    |head ys < head xs = head ys : mergeLinearRec xs (tail ys)
    |otherwise = head xs : mergeLinearRec (tail xs) (tail ys)

mergeXs :: [Int] -> [Int] -> [Int]
mergeXs xs ys = nub $ sort $ xs ++ ys