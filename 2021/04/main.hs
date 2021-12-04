import Data.List.Split(splitOn, splitWhen)
import Data.List (transpose)
import System.Environment (getArgs)
import Text.Html (content)

data Case = Value Int | Crossed deriving (Show, Read, Eq)

intToCase :: Int -> Case
intToCase = Value

isCrossed :: Case -> Bool
isCrossed Crossed = True
isCrossed (Value _) = False

caseToInt :: Case -> Int
caseToInt Crossed = 0
caseToInt (Value a) = a

equalsValue :: Int -> Case -> Bool
equalsValue _ Crossed = False
equalsValue a (Value b) = a == b

type BoardLine = [Case]
type Board = [BoardLine]
type Input = [String]

data WinningBoard = Board Board | None deriving (Show, Read, Eq)

extractBoard :: WinningBoard -> Board
extractBoard None = []
extractBoard (Board b) = b

-- get and process input

getInput :: FilePath -> IO Input
getInput path = do
    content <- readFile path
    return $lines content

getDraw :: Input -> [Int]
getDraw [] = []
getDraw (x:_) = map stringToInt $ splitOn "," x

stringToInt :: String -> Int
stringToInt x = read x :: Int

-- split on empty lines to separate boards
splitInput :: Input -> [[String]]
splitInput = tail . splitWhen (==[])

-- split on words to separate lines
explodeInput :: [[String]] -> [[[String]]]
explodeInput = (map . map) words

-- and let's get numbers
convertInput :: [[[String]]] -> [Board]
convertInput = (map . map . map) (intToCase . stringToInt)

getBoards :: Input -> [Board]
getBoards = convertInput . explodeInput . splitInput

-- solve part 1 problem
-- so many functions, quite sure it's too much

crossCase :: Int -> Case -> Case
crossCase int value
    | equalsValue int value = Crossed
    | otherwise = value

crossBoard :: Int -> Board -> Board
crossBoard a = (map . map) (crossCase a)

-- check a list representing a row or column
isWinningSequence :: BoardLine -> Bool
isWinningSequence [] = True
isWinningSequence (x:xs)
    | isCrossed x = isWinningSequence xs
    | otherwise = False

-- look for a completely crossed row
hasWinningLine :: Board -> Bool
hasWinningLine = foldr ((||) . isWinningSequence) False

-- check for a completely crossed row or column
isWinnerBoard :: Board -> Bool
isWinnerBoard b = hasWinningLine b || (hasWinningLine . transpose) b

-- is there a winning board in this list of boards?
findWinningBoard :: [Board] -> WinningBoard
findWinningBoard [] = None
findWinningBoard (x:xs)
    | isWinnerBoard x = Board x
    | otherwise = findWinningBoard xs

getWinningBoard :: [Int] -> [Board] -> (Int, Board)
getWinningBoard _ [] = (0, [])
getWinningBoard [] _ = (0, [])
getWinningBoard (x:xs) boards
    | winningBoard == None = getWinningBoard xs crossedBoards
    | otherwise = (x, extractBoard winningBoard)
    where crossedBoards = map (crossBoard x) boards
          winningBoard = findWinningBoard crossedBoards

sumUncrossedValues' :: BoardLine -> Int
sumUncrossedValues' = foldr ((+) . caseToInt) 0

sumUncrossedValues :: Board -> Int
sumUncrossedValues = foldr ((+) . sumUncrossedValues') 0

-- now we use all of that

main :: IO ()
main = do
    (path:_) <- getArgs
    content <- getInput path

    let draw = getDraw content
        boards = getBoards content
        (lastNumber, winningBoard) = getWinningBoard draw boards
    
    putStrLn $ "The last number called is: " ++ show lastNumber
    putStrLn $ "Ther winning board is: " ++ show winningBoard
    putStrLn $ "The answer for part 1 is:" ++ show (lastNumber * sumUncrossedValues winningBoard)
