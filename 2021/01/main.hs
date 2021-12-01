import System.Environment

main = do 
    (path:_) <- getArgs
    fileContent <- readFile path
    let values = map read (lines fileContent)
        answerPart1 = (length . getWhenDeeper . pairing) values
        answerPart2 = getAnswerPart2 values
    putStrLn $ "The answer for part 1 is: " ++ (show answerPart1)
    putStrLn $ "The answer for part 2 is: " ++ (show answerPart2)

pairing :: [Int] -> [(Int, Int)]
pairing [] = []
pairing [a] = []
pairing (x:y:xs) = (x,y) : pairing (y:xs)

getWhenDeeper :: [(Int, Int)] -> [(Int, Int)]
getWhenDeeper = filter (\(x,y) -> x < y)

makeTuples :: [Int] -> [(Int, Int, Int)]
makeTuples [] = []
makeTuples [a] = []
makeTuples [a, b] = []
makeTuples (x:y:z:xs) = (x,y,z) : makeTuples (y:z:xs)

sumTuple :: (Int, Int, Int) -> Int
sumTuple (a, b, c) = a + b + c

getAnswerPart2 :: [Int] -> Int
getAnswerPart2 = length . getWhenDeeper . pairing . (map sumTuple) . makeTuples