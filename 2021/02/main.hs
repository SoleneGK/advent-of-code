import System.Environment

type Direction = String
type HPosition = Int
type Depth = Int
type Aim = Int
type Instructions = (String, Int)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

getValuesFromFile :: FilePath -> IO [Instructions]
getValuesFromFile path = do
    contents <- readFile path
    return $ (convert . words) contents

convert :: [String] -> [Instructions]
convert [] = []
convert [x] = []
convert (x:y:xs) = (x, read y :: Int) : convert xs

getEndPositionPart1 :: [Instructions] -> (HPosition, Depth)
getEndPositionPart1 [] = (0, 0)
getEndPositionPart1 (("forward", x):xs) = add (x, 0) (getEndPositionPart1 xs)
getEndPositionPart1 (("up", x):xs) = add (0, -x) (getEndPositionPart1 xs)
getEndPositionPart1 (("down", x):xs) = add (0, x) (getEndPositionPart1 xs)
getEndPositionPart1 (_:xs) = getEndPositionPart1 xs

getEndPositionPart2 :: [Instructions] -> Aim -> (HPosition, Depth)
getEndPositionPart2 [] _ = (0, 0)
getEndPositionPart2 (("up", x):xs) aim = getEndPositionPart2 xs (aim - x)
getEndPositionPart2 (("down", x):xs) aim = getEndPositionPart2 xs (aim + x)
getEndPositionPart2 (("forward", x):xs) aim = add (x, x * aim) (getEndPositionPart2 xs aim)
getEndPositionPart2 (_:xs) aim = getEndPositionPart2 xs aim

main = do
    (path:_) <- getArgs
    instructions <- getValuesFromFile path
    let (hPosition, depth) = getEndPositionPart1 instructions
    putStrLn $ "Final horizontal position is: " ++ show hPosition
    putStrLn $ "Final depth is: " ++ show depth
    putStrLn $ "Answer for part 1 is: " ++ show (hPosition * depth)

    let (hPosition2, depth2) = getEndPositionPart2 instructions 0
    putStrLn $ "Final horizontal position is: " ++ show hPosition2
    putStrLn $ "Final depth is: " ++ show depth2
    putStrLn $ "Answer for part 2 is: " ++ show (hPosition2 * depth2)
