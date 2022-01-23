import Data.Char
import Data.List
main :: IO()
main = do
    -- Task 1 main driver
    print $ getSunk database  == [("Guadalcanal",["Kirishima"]),("North Atlantic",["Bismarck","Hood"]),("North Cape",["Schamhorst"]),("Surigao Strait",["Fuso","Yamashiro"])]
    print $ inBattleAfterDamaged database  == ["California","Prince of Wales"]
    -- Task 2 main driver
    print $ grandchildrenIncreased t1 == True
    print $ grandchildrenIncreased t2 == False

-- Definitions

type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int

data Battle = Battle Name Date
    deriving (Show)
data Ship = Ship Name Class Launched
    deriving (Show)
data Outcome = Outcome Name Name Result
    deriving (Show)
type Database = ([Outcome], [Battle], [Ship])

outcomes :: [Outcome]
outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", Outcome "California" "Surigao Strait" "ok", Outcome "Duke of York" "North Cape" "ok", Outcome "Fuso" "Surigao Strait" "sunk", Outcome "Hood" "North Atlantic" "sunk", Outcome "King George V" "North Atlantic" "ok", Outcome "Kirishima" "Guadalcanal" "sunk", Outcome "Prince of Wales" "North Atlantic" "damaged", Outcome "Rodney" "North Atlantic" "ok", Outcome "Schamhorst" "North Cape" "sunk", Outcome "South Dakota" "Guadalcanal" "damaged", Outcome "Tennessee" "Surigao Strait" "ok", Outcome "Washington" "Guadalcanal" "ok", Outcome "Prince of Wales" "Guadalcanal" "ok", Outcome "West Virginia" "Surigao Strait" "ok", Outcome "Yamashiro" "Surigao Strait" "sunk", Outcome "California" "Guadalcanal" "damaged" ]

battles :: [Battle]
battles = [ Battle "Guadalcanal" "1942-11-15", Battle "North Atlantic" "1941-05-25", Battle "North Cape" "1943-12-26", Battle "Surigao Strait" "1944-10-25" ]

ships :: [Ship]
ships = [ Ship "California" "Tennessee" 1921, Ship "Haruna" "Kongo" 1916, Ship "Hiei" "Kongo" 1914, Ship "Iowa" "Iowa" 1943, Ship "Kirishima" "Kongo" 1915, Ship "Kongo" "Kongo" 1913, Ship "Missouri" "Iowa" 1944, Ship "Musashi" "Yamato" 1942, Ship "New Jersey" "Iowa" 1943, Ship "North Carolina" "North Carolina" 1941, Ship "Ramillies" "Revenge" 1917, Ship "Renown" "Renown" 1916, Ship "Repulse" "Renown" 1916, Ship "Resolution" "Renown" 1916, Ship "Revenge" "Revenge" 1916, Ship "Royal Oak" "Revenge" 1916, Ship "Royal Sovereign" "Revenge" 1916, Ship "Tennessee" "Tennessee" 1920, Ship "Washington" "North Carolina" 1941, Ship "Wisconsin" "Iowa" 1944, Ship "Yamato" "Yamato" 1941, Ship "Yamashiro" "Yamato" 1947, Ship "South Dakota" "North Carolina" 1941, Ship "Bismarck" "North Carolina" 1911, Ship "Duke of York" "Renown" 1916, Ship "Fuso" "Iowa" 1940, Ship "Hood" "Iowa" 1942, Ship "Rodney" "Yamato" 1915, Ship "Yanashiro" "Yamato" 1918, Ship "Schamhorst" "North Carolina" 1917, Ship "Prince of Wales" "North Carolina" 1937, Ship "King George V" "Iowa" 1942, Ship "West Virginia" "Iowa" 1942 ]

database :: Database
database = (outcomes, battles, ships)

-- ----------------------------------------------------TASK_1--------------------------------------------------
--Part 1
--For every battle we need to traverse all outcomes and from them we need to traverse all ships which meet the criteria
getByOutcome :: Name -> [Outcome] -> [Name]
getByOutcome _ [] = []
getByOutcome battleName ((Outcome shipName bat res):battlesToSearch)
 | battleName == bat && res == "sunk" = shipName : getByOutcome battleName battlesToSearch
 | otherwise = getByOutcome battleName battlesToSearch

getByBattles :: Name -> Name -> [Outcome] -> (Name, [Name])
getByBattles name _ [] = (name,[])
getByBattles name date results = (name, getByOutcome name results)

getSunk :: Database -> [(Name, [Name])]
getSunk (outs,[],shippings) = []
getSunk (outs,((Battle name date):rests),shippings) = getByBattles name date outs : getSunk (outs, rests, shippings)

--Part 2
getSpecific :: [Outcome] -> Name -> [(Name,Name)]
getSpecific [] _ = []
getSpecific ((Outcome bat ship res):xs) category
 | res==category = (ship,bat):getSpecific xs category
 | otherwise = getSpecific xs category

getYear :: [Battle] -> Name -> Int
getYear ((Battle name date):rests) criteria
 | criteria==name = read (take 4 date)
 | otherwise = getYear rests criteria

mapYear :: [(Name,Name)] -> [Battle] -> [(Int,Name)]
mapYear [] _ = []
mapYear ((battle,ship):xs) battleArray = (getYear battleArray battle,ship):mapYear xs battleArray

findShip :: Int -> Name -> [(Int,Name)] -> Int
findShip _ _ [] = 0
findShip yearToSearch shipName ((yearBat,nameSh):xs)
 | shipName == nameSh && yearBat>yearToSearch = yearBat
 | otherwise = findShip yearToSearch shipName xs

convert :: [(Int,Name)] -> [(Int,Name)] -> [Name]
convert [] _ = []
convert ((yearDamage, shipNameDamage):brokens) fines
 | findShip yearDamage shipNameDamage fines > 0 = shipNameDamage : convert brokens fines
 | otherwise = convert brokens fines

inBattleAfterDamaged :: Database -> [Name]
inBattleAfterDamaged (outs, bats, shippings) = reverse $ convert (mapYear (getSpecific outs "damaged") bats) (mapYear (getSpecific outs "ok") bats)

-- ----------------------------------------------------TASK_2--------------------------------------------------
data BTree = Nil | Node Int BTree BTree
 deriving (Show,Eq)

t1=Node 1 (Node (-1) (Node 2 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)) (Node (-1) Nil Nil)
t2=Node 1 (Node 2 (Node 1 Nil Nil) (Node 1 (Node 10 Nil Nil) Nil)) (Node 3 Nil Nil)
t3=Node 2 (Node 3 Nil (Node 5 Nil Nil)) (Node 3 Nil Nil)

hasGrandFather :: BTree -> Bool
hasGrandFather (Node _ Nil right) = False
hasGrandFather (Node _ left Nil) = False
hasGrandFather (Node val (Node valL leftL rightL) (Node valR leftR rightR))
 | leftL/=Nil || rightL/=Nil ||leftR/=Nil ||rightR/=Nil = True
 |otherwise = False


grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Nil = True
grandchildrenIncreased (Node value left right) = (getGrandFatherValueIfExists left) && (getGrandFatherValueIfExists right)  && grandchildrenIncreased left && grandchildrenIncreased right
    where
        getGrandFatherValueIfExists :: BTree -> Bool
        getGrandFatherValueIfExists Nil = True
        getGrandFatherValueIfExists (Node _ Nil Nil) = True
        getGrandFatherValueIfExists (Node _ Nil (Node rightValToCheck Nil Nil)) = value < rightValToCheck
        getGrandFatherValueIfExists (Node _ (Node leftValToCheck Nil Nil) Nil) = value < leftValToCheck
        getGrandFatherValueIfExists (Node _ (Node leftValToCheck leftL rightL) (Node rightValToCheck leftR rightR)) = value < leftValToCheck || value < rightValToCheck