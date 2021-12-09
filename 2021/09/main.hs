import Data.Array
import Data.List.Split ( chunksOf )
import System.Environment (getArgs)

type Height = Int
type Heightmap = [(Coordinates, Height)]
type Coordinates = (X, Y)
type X = Int
type Y = Int

int :: String -> Int
int s = read s :: Int

readInput :: FilePath -> IO [[Height]]
readInput path = do
    rawInput <- readFile path
    return $ map (map int . chunksOf 1) (lines rawInput)

mapInput :: [[Height]] -> X -> Heightmap
mapInput [] _ = []
mapInput (l:ls) x = zip coordinates l ++ mapInput ls (x+1)
    where coordinates = [(x, y) | y <- [1..length l]]

isNeighbour :: Coordinates -> (Coordinates, Height) -> Bool
isNeighbour (x1, y1) ((x2, y2), _)
    | x1 == x2 && abs (y1 - y2) == 1 = True
    | abs (x1 - x2) == 1 && y1 == y2 = True
    | otherwise = False

getNeighbours :: Heightmap -> Coordinates -> Heightmap
getNeighbours hmap c = filter (isNeighbour c) hmap

isLowPoint :: Heightmap -> (Coordinates, Height) -> Bool
isLowPoint hmap (c, h) = null lowerNeighbours
    where n = getNeighbours hmap c
          lowerNeighbours = filter (\(c2, h2) -> h2 <= h) n

getLowPoints :: Heightmap -> Heightmap
getLowPoints hmap = filter (isLowPoint hmap) hmap

answerPart1 :: Heightmap -> Int
answerPart1 hmap = sum $ map (\(c, h) -> h+1) lowPoints
    where lowPoints = getLowPoints hmap

main :: IO ()
main = do
    (path:_) <- getArgs
    rawInput <- readInput path
    let hmap = mapInput rawInput 1
    putStrLn $ "The answer for part 1 is: " ++ show (answerPart1 hmap)