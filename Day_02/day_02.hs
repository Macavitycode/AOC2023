import Data.Char (isDigit)
import Data.List (isInfixOf, findIndices, elemIndices, sort, sortBy, groupBy)

splitD :: Char -> String -> [String]
splitD _ "" = []
splitD delimiter str =
    let (start, rest) = break (== delimiter) str
        (_, remain) = span (== delimiter) rest
     in start : splitD delimiter remain

getSubGames :: String -> [String]
getSubGames ls = splitD ';' (splitD ':' ls !! 1)

isSubConditionValid :: String -> Bool
isSubConditionValid str
    | "red"     `isInfixOf` str =  read (filter isDigit str) <= 12
    | "green"   `isInfixOf` str =  read (filter isDigit str) <= 13
    | "blue"    `isInfixOf` str =  read (filter isDigit str) <= 14

readAsToup :: String -> (Int, String)
readAsToup asd = (read (head (words asd)) :: Int, words asd !! 1)

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    putStrLn "Step 1:"
    print $ sum (map (+1) (elemIndices True (map ((minimum . map minimum) . map (map isSubConditionValid . splitD ',') . getSubGames) contents)))
    putStrLn "Step 2:"
    print $ sum (map (product . map (fst . maximum) . (groupBy (\(a, b) (x, y) -> b == y) . sortBy (\(a, b) (x, y) -> compare b y)) . concatMap (map readAsToup . splitD ',') . getSubGames) contents)

