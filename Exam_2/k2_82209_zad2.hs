main :: IO()
main = do
    print $ stocklist stocks ['A','B']  == [('A',200),('B',1140)]
    print $ stocklist stocks ['A','B']  == [('A',200),('B',1140)]
    print $ stocklist stocks ['C','X']  == [('C',500),('X',0)]
    print $ stocklist stocks ['Y','X']  == [('Y',0),('X',0)]
    print $ stocklist stocks ['C']  == [('C', 500)]

data Stock = Stock String Int
stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]
ex = [Stock "ABART" 20, Stock "CDXEF" 50, Stock "BKWRK" 25, Stock "BTSQZ" 89, Stock "DRTYM" 60]

-- here we only count the books if the result is 0 then no book from this category exists
isAvailable :: [Stock] -> Char -> Int
isAvailable [] _ = 0
isAvailable ((Stock name inStock):ls) searchSymbol
 | searchSymbol == name!!0 = inStock + isAvailable ls searchSymbol
 | otherwise = isAvailable ls searchSymbol

stocklist :: [Stock] -> [Char] -> [(Char,Int)]
stocklist [] _ = []
stocklist _ [] = []
stocklist books categories = helper books categories
    where
        helper :: [Stock] -> [Char] -> [(Char,Int)]
        helper [] (cat:xs) =  (cat,0) : stocklist [] xs -- if the books end print all categories with 0 fro availability
        helper ((Stock name inStock):ls) (cat:xs) = (cat,isAvailable books cat) : stocklist ls xs

-- can ending in two cases for [] be a problem?
-- or we can combine them into one because if we 
-- have ended with the stocks and we have more categories
-- to review they wont appear in the result

