import Data.List (foldl')
import Data.List.Split (splitOn) -- required `cabal install --lib split`

main = do
    input <- fmap lines $ readFile "input.txt"
    let crabPositions = map read $ splitOn "," $ head input
    let possibleAlignmentPositions = [minimum crabPositions .. maximum crabPositions]
    putStrLn $ "The answer to Part 1 is: " ++ (show $ part1 crabPositions possibleAlignmentPositions)
    putStrLn $ "The answer to Part 2 is: " ++ (show $ part2 crabPositions possibleAlignmentPositions)

findMinFuelCost :: (Int -> Int) -> [Int] -> [Int] -> Int
findMinFuelCost costFn positions alignmentPositions = minimum $ map getFuelCost alignmentPositions
    where getFuelCost alignmentPos = foldl' (\acc pos -> acc + costFn (abs (pos - alignmentPos))) 0 positions

part1 :: [Int] -> [Int] -> Int
part1 = findMinFuelCost (\distance -> distance)

part2 :: [Int] -> [Int] -> Int
part2 = findMinFuelCost (\distance -> distance * (distance + 1) `div` 2)