import System.Environment
import Data.List (transpose, partition)
import Data.Char(digitToInt)

type Column = [Char]
type BinNumber = [Int]

getValues :: FilePath -> IO [Column]
getValues path = do
    content <- readFile path
    return $ lines content

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

-- must be from least to most significant bit
convertBinToDecLSB :: BinNumber -> Int
convertBinToDecLSB [] = 0
convertBinToDecLSB [x] = x
convertBinToDecLSB (x:xs) = x + 2 * convertBinToDecLSB xs

convertBinToDec :: BinNumber -> Int
convertBinToDec = convertBinToDecLSB . reverse

--------------------------------------------------------------------

type Line = [Char]

getNthBits :: Int -> [Line] -> Column
getNthBits i = map (!!i)

-- If as many 1 as 0, return 1
getMostCommonBit :: Column -> Char 
getMostCommonBit c
    | numberOf0 > numberOf1 = '0'
    | otherwise = '1'
    where (numberOf0, numberOf1) = (countDigits . sortDigits) c

-- If as many 1 as 0, return 0
getLeastCommonBit :: Column -> Char
getLeastCommonBit c
    | numberOf0 <= numberOf1 = '0'
    | otherwise = '1'
    where (numberOf0, numberOf1) = (countDigits . sortDigits) c

filterByBit :: Char -> Int -> [Line] -> [Line]
filterByBit c i = filter (\l -> l!!i == c)

getOxGenRate :: [Line] -> Int -> Line
getOxGenRate [] _ = []
getOxGenRate [l] _ = l
getOxGenRate ll i = getOxGenRate filteredList (i+1)
    where filteredList = filterByBit (getMostCommonBit $ getNthBits i ll) i ll

getCo2ScrubRate :: [Line] -> Int -> Line
getCo2ScrubRate [] _ = []
getCo2ScrubRate [l] _ = l
getCo2ScrubRate ll i = getCo2ScrubRate filteredList (i+1)
    where filteredList = filterByBit (getLeastCommonBit $ getNthBits i ll) i ll

-- must be from least to most significant bit
convertLineToDecLSB :: Line -> Int
convertLineToDecLSB [] = 0
convertLineToDecLSB [c] = digitToInt c
convertLineToDecLSB (c:cs) = digitToInt c + 2 * convertLineToDecLSB cs

convertLineToDec :: Line -> Int
convertLineToDec = convertLineToDecLSB . reverse

main :: IO()
main = do
    (path:_) <- getArgs
    content <- getValues path

    let dataPart1 = transpose content
        (gammaRateBin, omegaRateBin) = getRates $ map (countDigits . sortDigits) dataPart1
        gammaRate = convertBinToDec gammaRateBin
        omegaRate = convertBinToDec omegaRateBin
    putStrLn $ "Gamma rate is: " ++ show gammaRate
    putStrLn $ "Omega rate is: " ++ show omegaRate
    putStrLn $ "Answer for part 1 is: " ++ show (gammaRate * omegaRate)

    let oxGenRate = convertLineToDec $ getOxGenRate content 0
        co2ScrubRate = convertLineToDec $ getCo2ScrubRate content 0
    putStrLn $ "Oxygen generator rating is: " ++ show oxGenRate
    putStrLn $ "CO2 scrubber rating is: " ++ show co2ScrubRate
    putStrLn $ "Answer for part 2 is: " ++ show (oxGenRate * co2ScrubRate)