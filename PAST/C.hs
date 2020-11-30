import Data.List (sort)


readInt :: String -> Int
readInt = read


splitBySpace :: String -> [String]
splitBySpace [] = [""]
splitBySpace (c:cs)
    | c == ' '  = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = splitBySpace cs


extractThird :: [Int] -> Int
extractThird ints = head $ drop 2 $ reverse $ sort ints


main :: IO ()
main = do
    input <- getLine
    let ints = [readInt x | x <- (splitBySpace input)]
    print $ extractThird ints
