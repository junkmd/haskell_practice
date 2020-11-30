import Control.Monad (replicateM)


getLines :: Int -> IO [String]
getLines n = replicateM n getLine


getInts :: Int -> IO [Int]
getInts n = fmap read <$> getLines n


getInt :: IO Int
getInt = fmap read getLine


solveIntsDiff :: [Int] -> [Int]
solveIntsDiff ints = zipWith (-) (drop 1 ints) ints


getDiffMsg :: Int -> String
getDiffMsg diff
    | diff == 0 = "stay"
    | diff > 0 = "up " ++ (show diff)
    | diff < 0 = "down " ++ (show $ abs diff)


join :: String -> [String] -> String
join _ [] = []
join _ [x] = x
join sep (x : xs) = x ++ sep ++ join sep xs


main :: IO ()
main = do
    days <- getInt
    sales <- getInts days
    let diffs = [getDiffMsg x | x <- (solveIntsDiff sales)]
    putStrLn $ join "\n" diffs
