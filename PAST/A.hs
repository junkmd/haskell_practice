import Text.Read (readMaybe)


err = "error"


readInt :: String -> Maybe Int
readInt = readMaybe


fromJust :: Maybe a -> a
fromJust (Just a) = a


doubleThreeDigits :: String -> String
doubleThreeDigits input
    | and[length input　== 3, maybe_int /= Nothing] = show $ (fromJust maybe_int) * 2
    | otherwise = err
    where maybe_int = readInt input


main :: IO ()
main = do
    input <- getLine
    putStrLn $ doubleThreeDigits input
