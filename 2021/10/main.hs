import System.Environment (getArgs)
import Data.Graph (path)
import Data.List (sort)
type Line = [Char]

data State = Incomplete [Char] | Corrupted Char deriving (Show, Eq)

isCorrupted :: State -> Bool
isCorrupted (Incomplete _) = False
isCorrupted (Corrupted _) = True

isIncomplete :: State -> Bool
isIncomplete = not . isCorrupted

getCompletionString :: State -> [Char]
getCompletionString (Corrupted _) = []
getCompletionString (Incomplete c) = c

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
getState expected [] = Incomplete expected
getState expected (l:ls)
    | isOpeningChar l = getState (closing l : expected) ls
    | null expected = Corrupted l
    | l == head expected = getState (tail expected) ls
    | otherwise = Corrupted l

getCorruptions :: [Line] -> [State]
getCorruptions lines = filter isCorrupted states
    where states = map (getState []) lines

getSyntaxErrorScore :: [State] -> Int
getSyntaxErrorScore [] = 0
getSyntaxErrorScore (s:ss)
    | s == Corrupted ')' = 3 + getSyntaxErrorScore ss
    | s == Corrupted ']' = 57 + getSyntaxErrorScore ss
    | s == Corrupted '}' = 1197 + getSyntaxErrorScore ss
    | otherwise = 25137 + getSyntaxErrorScore ss

getIncompletes :: [Line] -> [State]
getIncompletes lines = filter isIncomplete states
    where states = map (getState []) lines

getLineScore :: [Char] -> Int
getLineScore [] = 0
getLineScore (c:cs) = charScore + partialScore
    where partialScore = 5 * getLineScore cs
          charScore = case c of ')' -> 1
                                ']' -> 2
                                '}' -> 3
                                _ -> 4

getIncompleteLineScore :: [State] -> [Int]
getIncompleteLineScore = map (getLineScore . reverse . getCompletionString)

getMiddleScore :: [State] -> Int
getMiddleScore states = scoreList!!middleIndex
    where scoreList = sort $ getIncompleteLineScore states
          middleIndex = (length scoreList - 1) `div` 2

main :: IO ()
main = do
    (path:_) <- getArgs
    input <- getInput path
    let corruptions = getCorruptions input
        score = getSyntaxErrorScore corruptions
    putStrLn $ "The total syntax error score is: " ++ show score

    let incompletes = getIncompletes input
        score2 = getMiddleScore incompletes
    putStrLn $ "The middle autocomplete score is: " ++ show score2