import System.Environment

main = do 
    (path:_) <- getArgs
    fileContent <- readFile path
    let values = map read (lines fileContent)
        answerPart1 = getAnswerPart1 values
        answerPart2 = getAnswerPart2 values
    putStrLn $ "The answer for part 1 is: " ++ show answerPart1
    putStrLn $ "The answer for part 2 is: " ++ show answerPart2

pairing :: [Int] -> [(Int, Int)]
pairing [] = []
pairing [a] = []
pairing (x:y:xs) = (x,y) : pairing (y:xs)

getWhenDeeper :: [(Int, Int)] -> [(Int, Int)]
getWhenDeeper = filter (\(x,y) -> x < y)

getAnswerPart1 :: [Int] -> Int
getAnswerPart1 = length . getWhenDeeper . pairing

sumOf3 :: [Int] -> [Int]
sumOf3 [] = []
sumOf3 [a] = []
sumOf3 [a, b] = []
sumOf3 (x:y:z:xs) = (x+y+z) : sumOf3 (y:z:xs)

getAnswerPart2 :: [Int] -> Int
getAnswerPart2 = getAnswerPart1 . sumOf3