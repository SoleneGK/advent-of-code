import System.Environment
import Data.List

type Column = [Char]
type BinNumber = [Int]

getValues :: FilePath -> IO [Column]
getValues path = do
    content <- readFile path
    return $ (transpose . lines) content

sortDigits :: Column -> (Column, Column)
sortDigits = partition (=='0')

countDigits :: (Column, Column) -> (Int, Int)
countDigits (a, b) = (length a, length b)

getRates :: [(Int, Int)] -> (BinNumber, BinNumber)
getRates [] = ([], [])
getRates ((a, b):xs)
    | a > b = addDigits (0, 1) (getRates xs)
    | otherwise = addDigits (1, 0) (getRates xs)

addDigits :: (Int, Int) -> (BinNumber, BinNumber) -> (BinNumber, BinNumber)
addDigits (d1, d2) (n1, n2) = (d1:n1, d2:n2)

-- must be from least to most significant beat
convertBinToDecLSB :: BinNumber -> Int
convertBinToDecLSB [] = 0
convertBinToDecLSB [x] = x
convertBinToDecLSB (x:xs) = x + 2 * convertBinToDecLSB xs

convertBinToDec :: BinNumber -> Int
convertBinToDec = convertBinToDecLSB . reverse

main :: IO()
main = do
    (path:_) <- getArgs
    content <- getValues path
    let (gammaRateBin, omegaRateBin) = getRates $ map (countDigits . sortDigits) content
    let gammaRate = convertBinToDec gammaRateBin
    let omegaRate = convertBinToDec omegaRateBin
    putStrLn $ "Gamma rate is: " ++ show gammaRate
    putStrLn $ "Omega rate is: " ++ show omegaRate
    putStrLn $ "Answer for part 1 is: " ++ show (gammaRate * omegaRate)

