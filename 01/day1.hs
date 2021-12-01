import Data.List

main = do
    contents <- readFile "input.txt"
    let depths = map read (lines contents)
    putStrLn $ "The answer to Part 1 is: " ++ (show $ part1 depths)
    putStrLn $ "The answer to Part 2 is: " ++ (show $ part2 depths)

{- Fold over a list of depth measurements and count the number of times the depth increases -}
part1 :: [Int] -> Int
part1 = fst . foldl' countIncreases (0, maxBound) -- |maxBound| ensures the first element isn't counted as an increase
    where countIncreases (count, previous) current = if current > previous then (count+1, current) else (count, current)

{- Fold over a list of depth measurements and count the number of times the 3-element rolling average depth increases -}
part2 :: [Int] -> Int
part2 = fst . foldl' countIncreases (0, [])
    where   countIncreases (count, [x,y,z]) current =   if (sum [current,x,y]) > (sum [x,y,z])
                                                        then (count+1, [current,x,y]) else (count, [current,x,y])
            countIncreases (count, previousFew) current = (count, current:previousFew)