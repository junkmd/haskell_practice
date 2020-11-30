import Control.Monad (replicateM)
import Data.List (group, sort)


getLines :: Int -> IO [String]
getLines n = replicateM n getLine


getInts :: Int -> IO [Int]
getInts n = fmap read <$> getLines n


getInt :: IO Int
getInt = fmap read getLine


isMember :: Int -> [Int] -> Bool
isMember n [] = False
isMember n (x:xs)
    | n == x = True
    | otherwise = isMember n xs


getMissingInts :: [Int] -> [Int] -> [Int]
getMissingInts former latter = [x | x <- former, not (isMember x latter)]


getDuplicatedInts :: [Int] -> [Int]
getDuplicatedInts ints = [(head x) | x <- group (sort ints), length x > 1]


main :: IO ()
main = do
    len <- getInt
    ints <- getInts len
    let orig = [1..len]
    let missings = getMissingInts orig ints
    let duplicated = getDuplicatedInts ints
    if and[null missings, null duplicated] then
        putStrLn $ "Correct"
    else
        putStrLn $ (show $ head duplicated) ++ " " ++ (show $ head missings)
