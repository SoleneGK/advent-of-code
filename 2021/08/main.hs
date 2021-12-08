import Data.List.Split (splitOn)
import System.Environment (getArgs)

type Output = [Number]
type Number = String

getOutput :: String -> Output
getOutput s = words rawOutput
    where _:rawOutput:_ = splitOn " | " s

getOutputs :: FilePath -> IO [Output]
getOutputs path = do
    rawContent <- readFile path
    return $ map getOutput (lines rawContent)

is1478 :: Number -> Bool
is1478 n = l == 2 || l == 3 || l == 4 || l == 7
    where l = length n

numberOf1478 :: [Output] -> Int 
numberOf1478 o = sum $ map (length . filter is1478) o

main :: IO ()
main = do
    (path:_) <- getArgs
    outputs <- getOutputs path
    putStrLn $ "There are " ++ show (numberOf1478 outputs) ++ " 1, 4, 7 or 8 in the output values"