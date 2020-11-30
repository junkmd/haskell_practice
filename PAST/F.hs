import Data.Char (isLower, toUpper)
import Data.List (sortOn, span)


splitBySecondUpper :: String -> [String]
splitBySecondUpper xs = spl xs
    where
        spl (c : cs) | (xs, y : ys) <- span isLower cs = (c : xs++[y]) : spl ys
        spl [] = []


join :: String -> [String] -> String
join _ [] = []
join _ [x]  = x
join sep (x : xs) = x ++ sep ++ join sep xs

 
main :: IO()
main = do
    input <- getLine
    let splitted = splitBySecondUpper input
    let sorted = sortOn (map toUpper) splitted
    putStrLn $ join "" sorted
