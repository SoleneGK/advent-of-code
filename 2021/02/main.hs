import System.Environment

type Direction = String
type HPosition = Int
type Depth = Int
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

getEndPosition :: [Instructions] -> (HPosition, Depth)
getEndPosition [] = (0, 0)
getEndPosition (("forward", a):xs) = add (a, 0) (getEndPosition xs)
getEndPosition (("up", a):xs) = add (0, -a) (getEndPosition xs)
getEndPosition (("down", a):xs) = add (0, a) (getEndPosition xs)
getEndPosition (_:xs) = getEndPosition xs

main = do
    (path:_) <- getArgs
    instructions <- getValuesFromFile path
    let (hPosition, depth) = getEndPosition instructions
    putStrLn $ "Final horizontal position is: " ++ show hPosition
    putStrLn $ "Final depth is: " ++ show depth
    putStrLn $ "Answer for part 1 is: " ++ show (hPosition * depth)