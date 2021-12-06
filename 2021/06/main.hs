import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Graph (path)

type Timer = Int
type States = [Timer]
type StateTable = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

stringToInt :: String -> Int
stringToInt s = read s :: Int

getInitialState :: FilePath -> IO States
getInitialState path = do
    rawContent <- readFile path
    return $ (map stringToInt . splitOn ",") rawContent

----------- part 1

getOlder :: Timer -> States
getOlder 0 = [6, 8]
getOlder t = [t-1]

nextDay :: States -> States
nextDay = concatMap getOlder

getNDaysLater :: States -> Int -> States
getNDaysLater s 0 = s
getNDaysLater s n = getNDaysLater (nextDay s) (n-1)

----------- part 2

addInitialState :: StateTable -> Timer -> StateTable
addInitialState (s0, s1, s2, s3, s4, s5, s6, s7, s8) t
    | t == 0 = (s0 + 1, s1, s2, s3, s4, s5, s6, s7, s8)
    | t == 1 = (s0, s1 + 1, s2, s3, s4, s5, s6, s7, s8)
    | t == 2 = (s0, s1, s2 + 1, s3, s4, s5, s6, s7, s8)
    | t == 3 = (s0, s1, s2, s3 + 1, s4, s5, s6, s7, s8)
    | t == 4 = (s0, s1, s2, s3, s4 + 1, s5, s6, s7, s8)
    | t == 5 = (s0, s1, s2, s3, s4, s5 + 1, s6, s7, s8)
    | otherwise = (s0, s1, s2, s3, s4, s5, s6 + 1, s7, s8)

calculateInitialState :: States -> StateTable
calculateInitialState = foldl addInitialState (0, 0, 0, 0, 0, 0, 0, 0, 0)

calculateNextDay :: StateTable -> StateTable
calculateNextDay (s0, s1, s2, s3, s4, s5, s6, s7, s8) = (s1, s2, s3, s4, s5, s6, s7 + s0, s8, s0)

calculateNthDay :: StateTable -> Int -> StateTable
calculateNthDay s 0 = s
calculateNthDay s n = calculateNthDay (calculateNextDay s) (n-1)

getNumberOfFishs :: StateTable -> Int
getNumberOfFishs (s0, s1, s2, s3, s4, s5, s6, s7, s8) = s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8

main :: IO ()
main = do
    (path:_) <- getArgs
    initialState <- getInitialState path

    let numberOfLanternfishsAfter80Days = length $ getNDaysLater initialState 80
    putStrLn $ "After 80 days, there are " ++ show numberOfLanternfishsAfter80Days ++ " lanternfishs"

    let initialState2 = calculateInitialState initialState
        numberOfLanternfishsAfter256Days = getNumberOfFishs $ calculateNthDay initialState2 256
    putStrLn $ "After 256 days, there are " ++ show numberOfLanternfishsAfter256Days ++ " lanternfishs"

