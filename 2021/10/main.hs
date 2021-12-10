import System.Environment (getArgs)
import Data.Graph (path)
type Line = [Char]

data State = Incomplete | Corrupted Char deriving (Show, Eq)

isCorrupted :: State -> Bool
isCorrupted = (/=) Incomplete

getInput :: FilePath -> IO [Line]
getInput path = do
    content <- readFile path
    return $ lines content

isOpeningChar :: Char -> Bool
isOpeningChar c = c == '(' || c == '[' || c == '{' || c == '<'

closing :: Char -> Char
closing c
    | c == '(' = ')'
    | c == '[' = ']'
    | c == '{' = '}'
    | otherwise = '>'

getState :: [Char] -> Line -> State
getState _ [] = Incomplete
getState expected (l:ls)
    | isOpeningChar l = getState (closing l : expected) ls
    | null expected = Corrupted l
    | l == head expected = getState (tail expected) ls
    | otherwise = Corrupted l

getCorruptions :: [Line] -> [State]
getCorruptions lines = filter isCorrupted states
    where states = map (getState []) lines

getScore :: [State] -> Int
getScore [] = 0
getScore (s:ss)
    | s == Corrupted ')' = 3 + getScore ss
    | s == Corrupted ']' = 57 + getScore ss
    | s == Corrupted '}' = 1197 + getScore ss
    | otherwise = 25137 + getScore ss

main :: IO ()
main = do
    (path:_) <- getArgs
    input <- getInput path
    let corruptions = getCorruptions input
        score = getScore corruptions
    putStrLn $ "The total syntax error score is: " ++ show score