getValuesFromFile :: FilePath -> IO [Int]
getValuesFromFile path = do
    contents <- readFile path
    return $ (convertListToInt . lines) contents

convertListToInt :: [String] -> [Int]
convertListToInt = map read