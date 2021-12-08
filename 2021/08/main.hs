import Data.List (sort, (\\), group, delete)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

type Output = [Digit]
type Digit = String
type InputLine = ([Digit], Output)
type DigitValue = (Digit, Int)

type Segment = Char

type Translator = [DigitValue]

--------------- part 1 ---------------

getOutput :: String -> Output
getOutput s = words rawOutput
    where _:rawOutput:_ = splitOn " | " s

getOutputs :: FilePath -> IO [Output]
getOutputs path = do
    rawContent <- readFile path
    return $ map getOutput (lines rawContent)

is1478 :: Digit -> Bool
is1478 n = l == 2 || l == 3 || l == 4 || l == 7
    where l = length n

numberOf1478 :: [Output] -> Int
numberOf1478 o = sum $ map (length . filter is1478) o

--------------- part 2 ---------------

-- values for original pattern are capitalized: ABCDEFGH

getLineOfData :: String -> InputLine
getLineOfData l = (words rawDigits, (map sort . words) rawOutput)
    where rawDigits:rawOutput:_ = splitOn " | " l

getInput :: FilePath -> IO [InputLine]
getInput path = do
    rawInput <- readFile path
    return $ map getLineOfData (lines rawInput)

identify1478 :: [Digit] -> Translator
identify1478 [] = []
identify1478 (d:ds)
    | l == 2 = (d, 1) : identify1478 ds
    | l == 3 = (d, 7) : identify1478 ds
    | l == 4 = (d, 4) : identify1478 ds
    | l == 7 = (d, 8) : identify1478 ds
    | otherwise = identify1478 ds
    where l = length d

getDigitValue :: Translator -> Int -> Digit
getDigitValue [] _ = ""
getDigitValue (d:ds) i
    | snd d == i = fst d
    | otherwise = getDigitValue ds i

getTranslator :: [Digit] -> Translator
getTranslator ds = [(zero, 0), (one, 1), (two, 2), (three, 3), (four, 4), (five, 5), (six, 6), (seven, 7), (eight, 8), (nine, 9)]
    where usedSegments = (group . sort . concat) ds
          digitValues = identify1478 ds

          -- 7 uses segments ACF, 1 uses segments CF
          -- by deduction, we get segment A
          seven = sort $ getDigitValue digitValues 7
          one = sort $ getDigitValue digitValues 1
          segmentA = head (seven \\ one)

          filterByLength l = filter (\x -> length x == l)

          -- segments B, E and F are the only ones used exactly 6, 4 and 9 times
          segmentB = (head . head) $ filterByLength 6 usedSegments
          segmentE = (head . head) $ filterByLength 4 usedSegments
          segmentF = (head . head) $ filterByLength 9 usedSegments

          -- segments A and C are used 8 times, we already know the association for A
          segmentsAC = map head $ filterByLength 8 usedSegments
          segmentC = head $ delete segmentA segmentsAC

          -- 4 uses segments BCDF, we know associations for B, C and F
          four = sort $ getDigitValue digitValues 4
          segmentD = head $ delete segmentB $ delete segmentC $ delete segmentF four

          -- segment G is the last one, we use the only number we know that contains it
          eight = sort $ getDigitValue digitValues 8
          segmentG = head $ delete segmentA $ delete segmentB $ delete segmentC $ delete segmentD $ delete segmentE $ delete segmentF eight

          zero = sort [segmentA, segmentB, segmentC, segmentE, segmentF, segmentG]
          two = sort [segmentA, segmentC, segmentD, segmentE, segmentG]
          three = sort [segmentA, segmentC, segmentD, segmentF, segmentG]
          five = sort [segmentA, segmentB, segmentD, segmentF, segmentG]
          six = sort [segmentA, segmentB, segmentD, segmentE, segmentF, segmentG]
          nine = sort [segmentA, segmentB, segmentC, segmentD, segmentF, segmentG]

translateDigit :: Translator -> Digit -> Int
translateDigit [] _ = 0
translateDigit (t:ts) d
    | fst t == d = snd t
    | otherwise = translateDigit ts d

getDisplayValue :: Translator -> Output -> Int
getDisplayValue t [a, b, c, d] = 1000 * translateDigit t a + 100 * translateDigit t b + 10 * translateDigit t c + translateDigit t d
getDisplayValue _ _ = 0

translateLine :: InputLine -> Int
translateLine l = getDisplayValue translator output
    where allDigits = fst l
          output = snd l
          translator = getTranslator allDigits


---------------- main ----------------

main :: IO ()
main = do
    (path:_) <- getArgs
    outputs <- getOutputs path
    putStrLn $ "There are " ++ show (numberOf1478 outputs) ++ " 1, 4, 7 or 8 in the output values"

    input <- getInput path
    let answerPart2 = sum $ map translateLine input
    putStrLn $ "The answer for part 2 is: " ++ show answerPart2