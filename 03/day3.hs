import Data.Char (digitToInt)
import Data.List (foldl', transpose)

main = do
    lines <- fmap lines $ readFile "input.txt"
    putStrLn $ "The answer to Part 1 is: " ++ (show $ part1 lines)
    putStrLn $ "The answer to Part 2 is: " ++ (show $ part2 lines)

type OneZeroComparison = Ordering

compareOnesToZeros :: String -> OneZeroComparison
compareOnesToZeros bits =   let ones = length $ filter (=='1') bits
                                zeros = length $ filter (=='0') bits
                            in ones `compare` zeros

getGamma :: [OneZeroComparison] -> String
getGamma = map mostCommon
    where   mostCommon GT = '1'
            mostCommon LT = '0'
            -- Don't handle EQ because the problem doesn't say how to handle it

part1 :: [String] -> Int
part1 lines =   let positionComparisons = map compareOnesToZeros (transpose lines)
                    gammaStr = getGamma positionComparisons
                    epsilonStr = [if char == '1' then '0' else '1' | char <- gammaStr]
                in (parseBinary gammaStr) * (parseBinary epsilonStr)

calculateRating :: [String] -> Int -> (OneZeroComparison -> Char) -> Int
calculateRating [s] _ _ = parseBinary s -- Stop when there's only one binary string left
calculateRating ss index getFilterChar =    let comparisonForIndex = compareOnesToZeros $ (transpose ss) !! index
                                                filterChar = getFilterChar comparisonForIndex
                                                filteredSs = filter (\s -> (s !! index) == filterChar) ss
                                            in calculateRating filteredSs (index + 1) getFilterChar

getOxygenGenRating :: [String] -> Int
getOxygenGenRating lines = calculateRating lines 0 keepMostCommon
    where   keepMostCommon :: OneZeroComparison -> Char
            keepMostCommon GT = '1'
            keepMostCommon EQ = '1'
            keepMostCommon LT = '0'

getCO2ScrubRating :: [String] -> Int
getCO2ScrubRating lines = calculateRating lines 0 keepLeastCommon
    where   keepLeastCommon :: OneZeroComparison -> Char
            keepLeastCommon GT = '0'
            keepLeastCommon EQ = '0'
            keepLeastCommon LT = '1'

part2 :: [String] -> Int
part2 lines = (getOxygenGenRating lines) * (getCO2ScrubRating lines)

-- Clever way to convert a binary number represented as a string to an int. Credit to iceman on Stack Overflow!
parseBinary :: String -> Int
parseBinary = foldl' (\acc x -> acc * 2 + digitToInt x) 0
