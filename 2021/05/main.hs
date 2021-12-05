import Data.List.Split (splitOn)
import Data.List (sort, group)
import System.Environment (getArgs)

newtype Coordinates = Coordinates (Int, Int) deriving (Show, Eq, Ord)
type Line = (Coordinates, Coordinates)

getContent :: FilePath -> IO [Line]
getContent path = do
    rawContent <- readFile path
    return $ (map extractLine . lines) rawContent

stringToInt :: String -> Int
stringToInt s = read s :: Int

convertToCoordinates :: [String] -> Coordinates
convertToCoordinates (x:y:xs) = Coordinates (stringToInt x, stringToInt y)
convertToCoordinates _ = Coordinates (0, 0)

extractCoordinates :: String -> Coordinates
extractCoordinates = convertToCoordinates . splitOn ","

convertToLine :: [String] -> Line
convertToLine (x:y:xs) = (extractCoordinates x, extractCoordinates y)
convertToLine _ = (Coordinates (0, 0), Coordinates (0, 0))

extractLine :: String -> Line
extractLine = convertToLine . splitOn " -> "

----- solve part 1

filterStraightLines :: [Line] -> [Line]
filterStraightLines = filter (\(Coordinates (x1,y1), Coordinates (x2, y2)) -> x1 == x2 || y1 == y2)

getSetOfCoordinates :: [Int] -> [Int] -> [Coordinates]
getSetOfCoordinates [] _ = []
getSetOfCoordinates _ [] = []
getSetOfCoordinates (x:xs) (y:ys) = Coordinates (x, y) : getSetOfCoordinates xs ys

getCoordinatesOnLine :: Line -> [Coordinates]
getCoordinatesOnLine (Coordinates (x1, y1), Coordinates (x2, y2))
    | x1 == x2 = getSetOfCoordinates (repeat x1) [min y1 y2..max y1 y2]
    | otherwise = getSetOfCoordinates [min x1 x2..max x1 x2] (repeat y2)

getAllCoordinates :: [Line] -> [Coordinates]
getAllCoordinates = concatMap getCoordinatesOnLine

getNumberOfOverlaps :: [Coordinates] -> Int
getNumberOfOverlaps c = length $ filter (\l -> length l >= 2) $ (group . sort) c

----- now use everything

main :: IO ()
main = do
    (path:_) <- getArgs
    content <- getContent path
    let answerPart1 = (getNumberOfOverlaps . getAllCoordinates . filterStraightLines) content
    putStrLn $ "The answer for part 1 is: " ++ show answerPart1