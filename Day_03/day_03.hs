import Data.Char (isDigit)

lookUpXY :: Int -> Int -> [[Char]] -> Char
lookUpXY x y grid
    | x >= 0 && y >= 0 && x < 140 && y < 140 = (grid !! y) !! x
    | otherwise = '\0'

-- Takes in line of chars and shits out list of numbers AS CHARS
getLineNums :: [Char] -> [((Int, Int), [Char])] -> Int -> Int -> [Char] -> [((Int, Int), [Char])]
getLineNums curList bigList xx yy line@(x:xs)
    | isDigit x     = getLineNums (curList ++ [x]) bigList (xx + 1) yy xs
    | curList /= "" = getLineNums "" (bigList ++ [((xx - length curList, yy), curList)]) (xx + 1) yy xs
    | otherwise     = getLineNums curList bigList (xx + 1) yy xs
getLineNums curList bigList xx yy ""
    | curList /= "" = bigList ++  [((xx - length curList, yy), curList)]
    | otherwise     = bigList

fullParseDesu :: [String] -> [[((Int, Int), [Char])]]
fullParseDesu = zipWith (getLineNums "" [] 0) [0..]

getNeighsCoords :: ((Int, Int), [Char]) -> [(Int, Int)]
getNeighsCoords ((xx, yy), str) = [(x, y) | x <- [xx - 1 .. xx + length str], y <- [yy - 1, yy + 1]] ++ [(xx - 1, yy), (xx + length str, yy)]

getNeighsChars :: ((Int, Int), [Char]) -> [[Char]] -> [Char]
getNeighsChars ((xx, yy), str) conts = filter ( /= '\0') (map (\(x, y) -> lookUpXY x y conts) $ getNeighsCoords ((xx, yy), str))

isPartNumber :: ((Int, Int), [Char]) -> [[Char]] -> Bool
isPartNumber  ((xx, yy), str) conts = any (\c -> c /='.' && not (isDigit c)) $ getNeighsChars ((xx, yy), str) conts

main :: IO ()
main = do
    contents <- lines <$> readFile "input.txt"
    let a = fullParseDesu contents
    print $ sum (map ((read :: String -> Int) . snd) (concatMap (filter (`isPartNumber` contents)) a))