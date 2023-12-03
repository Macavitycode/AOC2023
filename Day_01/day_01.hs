import Data.Char (isDigit)
import Data.List ( isPrefixOf )

convertChars :: String -> String
convertChars bigList@(sublist:list)
    | "one"     `isPrefixOf` bigList = '1':convertChars list
    | "two"     `isPrefixOf` bigList = '2':convertChars list
    | "three"   `isPrefixOf` bigList = '3':convertChars list
    | "four"    `isPrefixOf` bigList = '4':convertChars list
    | "five"    `isPrefixOf` bigList = '5':convertChars list
    | "six"     `isPrefixOf` bigList = '6':convertChars list
    | "seven"   `isPrefixOf` bigList = '7':convertChars list
    | "eight"   `isPrefixOf` bigList = '8':convertChars list
    | "nine"    `isPrefixOf` bigList = '9':convertChars list
    | isDigit sublist                = sublist:convertChars list 
    | otherwise                      = convertChars list
convertChars "" = ""

getNumbersAsCharList :: String -> String
getNumbersAsCharList list = [x | x <- list, isDigit x]

dropMiddle :: String -> String
dropMiddle list = head list : [last list]

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    putStrLn "Step 1:"
    print $ sum (map (read . dropMiddle . getNumbersAsCharList) contents)
    putStrLn "Step 2:"
    print $ sum (map (read . dropMiddle . getNumbersAsCharList . convertChars) contents)
