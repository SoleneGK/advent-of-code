import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Data.Graph (path)

type Timer = Int
type States = [Timer]

stringToInt :: String -> Int
stringToInt s = read s :: Int

getInitialState :: FilePath -> IO States
getInitialState path = do
    rawContent <- readFile path
    return $ (map stringToInt . splitOn ",") rawContent

getOlder :: Timer -> States
getOlder 0 = [6, 8]
getOlder t = [t-1]

nextDay :: States -> States
nextDay = concatMap getOlder

getNDaysLater :: States -> Int -> States
getNDaysLater s 0 = s
getNDaysLater s n = getNDaysLater (nextDay s) (n-1)

main :: IO ()
main = do
    (path:_) <- getArgs
    initialState <- getInitialState path
    let numberOfLanternfishs = length $ getNDaysLater initialState 80
    putStrLn $ "After 80 days, there are " ++ show numberOfLanternfishs ++ " lanternfishs"