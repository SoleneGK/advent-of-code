import Data.List.Split (splitOn)
import Data.List (group, sort)
import System.Environment (getArgs)

type Position = Int
type PositionList = [(Position, Int)]

stringToInt :: String -> Int
stringToInt s = read s :: Int

getPositions :: FilePath -> IO [Position]
getPositions path = do
    rawData <- readFile path
    return $ map stringToInt (splitOn "," rawData)

getPositionData :: [Position] -> (Position, Int)
getPositionData [] = (0, 0)
getPositionData (p:ps) = (p, length ps + 1)

getPositionList :: [Position] -> PositionList
getPositionList = map getPositionData . group . sort

maxPosition :: PositionList -> Int
maxPosition l = p
    where (p, _) = last l

getFuelCostForPosition :: Int -> (Position, Int) -> Int
getFuelCostForPosition i (p, nb) = abs (p - i) * nb

getFuelCost :: Int -> PositionList -> Int
getFuelCost i = sum . map (getFuelCostForPosition i)

getAllFuelCosts :: Int -> PositionList -> [Int]
getAllFuelCosts i p
    | i < 0 = []
    | otherwise = getFuelCost i p : getAllFuelCosts (i-1) p

getMinimalFuelCost :: PositionList -> Int
getMinimalFuelCost l = minimum $ getAllFuelCosts (maxPosition l) l

---------- part 2

getFuelCostForPositionPart2 :: Int -> (Position, Int) -> Int
getFuelCostForPositionPart2 i (p, nb) = nb * unitCost
    where distance = abs (p - i)
          unitCost = distance * (distance + 1) `div` 2

getFuelCostPart2 :: Int -> PositionList -> Int
getFuelCostPart2 i = sum . map (getFuelCostForPositionPart2 i)

getAllFuelCostsPart2 :: Int -> PositionList -> [Int]
getAllFuelCostsPart2 i p
    | i < 0 = []
    | otherwise = getFuelCostPart2 i p : getAllFuelCostsPart2 (i-1) p

getMinimalFuelCostPart2 :: PositionList -> Int
getMinimalFuelCostPart2 l = minimum $ getAllFuelCostsPart2 (maxPosition l) l


main :: IO ()
main = do
    (path:_) <- getArgs
    rawData <- getPositions path
    let positions = getPositionList rawData
    putStrLn $ "The minimal cost of fuel for part 1 is: " ++ show (getMinimalFuelCost positions)
    putStrLn $ "The minimal cost of fuel for part 2 is: " ++ show (getMinimalFuelCostPart2 positions)
