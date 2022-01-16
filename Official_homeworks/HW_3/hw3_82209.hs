import Data.Char
import Data.List
main :: IO()
main = do
    print $ formatDuration 0 == "now"
    print $ formatDuration 1 == "1 second"
    print $ formatDuration 62 == "1 minute and 2 seconds"
    print $ formatDuration 120 == "2 minutes"
    print $ formatDuration 3600 == "1 hour"
    print $ formatDuration 3662 == "1 hour, 1 minute and 2 seconds"
    --print $ formatDuration (-3662) == "1 hour, 1 minute and 2 seconds"
    print $ willItFly [1, 4, 2, 3] == True -- ➝ True -- |1-4|=3,|4-2|=2,|2-3|=1
    print $ willItFly [1, 4, 2, -1, 6] == False-- ➝ False -- |1-4|=3, |4-2|=2, |2+1|=3, |-1-6|=7
-- ----------------------------------------------------TASK_1--------------------------------------------------

--make list from all numbers from 1 to length of the list -1
getAllInterval :: Int -> [Int]
getAllInterval number = [1 .. number-1]

-- main driver
willItFly :: [Int] -> Bool
willItFly xs = helper xs (getAllInterval (length xs))
    where 
        helper :: [Int]->[Int]->Bool
        helper gifts remainingNumbers
         |(length gifts == 1 ) || null remainingNumbers = True
         |abs ((gifts!!0)-(gifts!!1)) `elem` remainingNumbers = helper (tail gifts) (delete (abs ((gifts!!0)-(gifts!!1))) remainingNumbers)
         |otherwise = False


-- ----------------------------------------------------TASK_2--------------------------------------------------

--Define some constants
secondsInOneYear :: Int --represent all seconds in a year
secondsInOneYear = 60 * 60 * 24 * 365
secondsInOneMonth :: Int --represent all seconds in one month
secondsInOneMonth = 60 * 60 * 24 * 30
secondsInOneWeek :: Int --represent all seconds in one week
secondsInOneWeek = 60 * 60 * 24 * 7
secondsInOneDay :: Int --represent all seconds in one day
secondsInOneDay = 60 * 60 * 24
secondsInOneHour :: Int --represent all seconds in one hour
secondsInOneHour = 60 * 60

--get years by seconds if they are zero empty string is returned.
getYears :: Int -> String
getYears seconds
 | seconds `div` secondsInOneYear == 0 = ""
 | seconds `div` secondsInOneYear == 1 = "1 year"
 | otherwise = show(seconds `div` secondsInOneYear) ++ " years"

--get months by seconds if they are zero empty string is returned.
getMonths :: Int -> String
getMonths seconds
 |((seconds `mod` secondsInOneYear) `div` secondsInOneMonth) == 0 = ""
 | ((seconds `mod` secondsInOneYear) `div` secondsInOneMonth) == 1 = "1 month"
 | otherwise = show((seconds `mod` secondsInOneYear) `div` secondsInOneMonth) ++ " months"

--get days by seconds if they are zero empty string is returned.
getDays :: Int -> String
getDays seconds
 | (((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `div` secondsInOneDay == 0 = ""
 | (((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `div` secondsInOneDay == 1 = "1 day"
 | otherwise = show((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `div` secondsInOneDay) ++ " days"

--get hours by seconds if they are zero empty string is returned.
getHours :: Int -> String
getHours seconds
 | ((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `div` secondsInOneHour == 0 = ""
 | ((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `div` secondsInOneHour == 1 = "1 hour"
 | otherwise = show(((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `div` secondsInOneHour) ++ " hours"

--get minutes by seconds if they are zero empty string is returned.
getMinutes :: Int -> String
getMinutes seconds
 | (((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `mod` secondsInOneHour) `div` 60 == 0 = ""
 | (((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `mod` secondsInOneHour) `div` 60 == 1 = "1 minute"
 | otherwise = show((((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `mod` secondsInOneHour) `div` 60) ++ " minutes"

--get seconds by seconds if they are zero empty string is returned.
getSeconds :: Int -> String
getSeconds seconds
 | (((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `mod` secondsInOneHour) `mod` 60 == 0 = ""
 | (((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `mod` secondsInOneHour) `mod` 60 == 1 = "1 second"
 | otherwise = show((((((seconds `mod` secondsInOneYear) `mod` secondsInOneMonth ) `mod` secondsInOneWeek) `mod` secondsInOneDay) `mod` secondsInOneHour) `mod` 60) ++ " seconds"

-- form list of all functions prioritized by longest period of time - year to the smallest one - seconds
functionList :: [Int -> String]
functionList = [getYears,getMonths,getDays,getHours,getMinutes,getSeconds]

-- apply the seconds on all functions and form a string of the result 
getStringList :: Int -> [String]
getStringList totalSeconds = map ($ totalSeconds) functionList

--concatenate strings if more than one is different from ""(empty string)
stringify::[String] -> String
stringify xs
 |null xs = ""
 |length xs == 2 = (xs!!0) ++" and "++ (xs!!1) ++ stringify []
 |otherwise = (head xs) ++ ", "++stringify (tail xs)

-- main driver
formatDuration :: Int -> String
formatDuration seconds
 | seconds <0 = error "Time can not be negative number"
 | all (\ x -> x=="")  (getStringList seconds) = "now" 
 | length(filter (\ x -> x/="") (getStringList seconds)) == 1 = unwords(filter (\ x -> x/="") (getStringList seconds)) --if there is only one specifier (no need of , and "and")
 | otherwise = stringify(filter (\ x -> x/="") (getStringList seconds)) -- if there are many specifiers (have to put , and "and")

-- Another possible solution is to start with one only one "variable" and then to initialize the rest(following the above implementation) but a problem 
-- may occur later when we concatenate all parts together
-- 