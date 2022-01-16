import Data.List

main :: IO()
main = do
    print $ matching "1234"  == []
    print $ matching ",[.[-],]"  == [(1,7),(3,5)]
    print $ matching ",+[-.,+]"  == [(2,7)]
    print $ matching "[][]" == [(0,1),(2,3)] --problem it does not work with not nested brackets make a func for checking a 

getAllFrontBracketPositions :: String -> [Int]
getAllFrontBracketPositions [] = []
getAllFrontBracketPositions expr = elemIndices '[' expr

getAllBackBracketPositions :: String -> [Int]
getAllBackBracketPositions [] = []
getAllBackBracketPositions expr =  elemIndices ']' expr

combine :: [Int] -> [Int] -> [(Int,Int)]
combine [] _ = [] -- do we need to add the rest of them with 0 ex. the bracket are not balanced
combine _ [] = [] -- do we need to add the rest of them with 0 ex. the bracket are not balanced
combine (x:xs) (y:ys) = (x,y) : combine xs ys

matching :: String -> [(Int, Int)]
matching [] = [] -- it is covered by the second case but with not relation to other functions(speed up the program as a whole?)
matching expr = combine (getAllFrontBracketPositions expr)  (reverse (getAllBackBracketPositions expr))
