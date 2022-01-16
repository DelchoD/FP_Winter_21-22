
import Data.List
import Data.Char
main :: IO()
main = do
    print $ squareDigits 9119 == 811181 -- ➝ 811181
    print $ squareDigits (-9119) == -811181

getSquare :: Char -> Int
getSquare = (^2) . digitToInt

squareDigits :: Int -> Int
squareDigits number
 | number>=0  = read $ concat $ map show( map getSquare (show number))
 | otherwise = -(read $ concat $ map show( map getSquare (drop 1 (show number))))

-- In case we wont use smth like +387; in this case we can make one more guard in the otherwise case where we will check if 
-- if head (show number) is + or - and on this we will make the result