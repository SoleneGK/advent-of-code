import Data.Array
import Data.List.Split (chunksOf)
import System.Environment (getArgs)

type X = Int
type Y = Int
type Coordinates = (X, Y)

type Height = Int
type Heightmap = Array Coordinates Height
type Heightlist = [(Coordinates, Height)]


int :: String -> Int
int s = read s :: Int

readInput :: FilePath -> IO [[Height]]
readInput path = do
    rawInput <- readFile path
    return $ map (map int . chunksOf 1) (lines rawInput)

mapInput :: [[Height]] -> X -> Heightlist
mapInput [] _ = []
mapInput (l:ls) x = zip coordinates l ++ mapInput ls (x+1)
    where coordinates = [(x, y) | y <- [1..length l]]

getHeightmap :: [[Height]] -> Heightmap
getHeightmap rawInput = array ((1, 1), (maxX, maxY)) heightData
    where maxX = length rawInput
          maxY = length $ head rawInput
          heightData = mapInput rawInput 1

getNeighbours :: Heightmap -> Coordinates -> Heightlist
getNeighbours hmap (x, y)
    | x == 1 && y == 1 = [(cBot, hmap!cBot), (cRight, hmap!cRight)]
    | x == 1 && y == maxY = [(cBot, hmap!cBot), (cLeft, hmap!cLeft)]
    | x == 1 = [(cBot, hmap!cBot), (cLeft, hmap!cLeft), (cRight, hmap!cRight)]
    | x == maxX && y == 1 = [(cTop, hmap!cTop), (cRight, hmap!cRight)]
    | x == maxX && y == maxY = [(cTop, hmap!cTop), (cLeft, hmap!cLeft)]
    | x == maxX = [(cTop, hmap!cTop), (cLeft, hmap!cLeft), (cRight, hmap!cRight)]
    | y == 1 = [(cTop, hmap!cTop), (cBot, hmap!cBot), (cRight, hmap!cRight)]
    | y == maxY = [(cTop, hmap!cTop), (cBot, hmap!cBot), (cLeft, hmap!cLeft)]
    | otherwise = [(cTop, hmap!cTop), (cBot, hmap!cBot), (cLeft, hmap!cLeft), (cRight, hmap!cRight)]
    where (maxX, maxY) = snd $ bounds hmap
          cTop = (x-1, y)
          cBot = (x+1, y)
          cLeft = (x, y-1)
          cRight = (x, y+1)

isLowPoint :: Heightmap -> (Coordinates, Height) -> Bool
isLowPoint hmap (c, h) = null lowerNeighbours
    where n = getNeighbours hmap c
          lowerNeighbours = filter (\(c2, h2) -> h2 <= h) n

getLowPoints :: Heightmap -> Heightlist
getLowPoints hmap = filter (isLowPoint hmap) hmapAsList
    where hmapAsList = [((x, y),hmap!(x, y)) | x <- [1..maxX], y <- [1..maxY]]
          (maxX, maxY) = snd $ bounds hmap

answerPart1 :: Heightmap -> Int
answerPart1 hmap = sum $ map (\(c, h) -> h+1) lowPoints
    where lowPoints = getLowPoints hmap

main :: IO ()
main = do
    (path:_) <- getArgs
    rawInput <- readInput path
    let hmap = getHeightmap rawInput
    putStrLn $ "The answer for part 1 is: " ++ show (answerPart1 hmap)