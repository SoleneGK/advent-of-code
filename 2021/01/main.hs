import System.Environment

main = do 
    (path:_) <- getArgs
    fileContent <- readFile path
    let values = map read (lines fileContent)
        answerPart1 = (length . getWhenDeeper . pairing) values
    putStrLn $ "The answer for part 1 is: " ++ (show answerPart1)

pairing :: [Int] -> [(Int, Int)]
pairing [] = []
pairing [a] = []
pairing (x:y:xs) = (x,y) : pairing (y:xs)

getWhenDeeper :: [(Int, Int)] -> [(Int, Int)]
getWhenDeeper = filter (\(x,y) -> x < y)
