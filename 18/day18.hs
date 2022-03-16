import Data.Char (digitToInt)
import Data.List (foldl', foldl1')


data SFN = Num Int | Pair (SFN, SFN) deriving (Show) -- SnailFish Number

type ExplodedPair = (Int, SFN)


main = do
    input <- fmap ((map parseSnailFishNumber) . lines) $ readFile "input.txt"
    let enumeratedInput = zip [0..] input
    let part1 = magnitude $ foldl1' add input
    let part2 = maximum $ [magnitude $ add a b | (i, a) <- enumeratedInput, (j, b) <- enumeratedInput, i /= j]
    putStrLn $ "The answer to Part 1 is " ++ (show $ part1)
    putStrLn $ "The answer to Part 2 is " ++ (show $ part2)


parseSnailFishNumber :: String -> SFN
parseSnailFishNumber [c] = Num $ digitToInt c
parseSnailFishNumber ('[' : rest) = let content = init rest -- Strip the trailing ']'
                                        (left, right) = splitOnCorrectComma content
                                    in  Pair (parseSnailFishNumber left, parseSnailFishNumber right)

splitOnCorrectComma :: String -> (String, String)
splitOnCorrectComma = impl 0 ""
    where impl depth before (c : after)
            | depth == 0 && c == ',' = (before, after)
            | c == '[' = impl (depth + 1) (before ++ [c]) after
            | c == ']' = impl (depth - 1) (before ++ [c]) after
            | otherwise = impl depth (before ++ [c]) after


-- Find a pair to explode, replace it with 0, and return it (with its number index).
doExplodePart1 :: (Int, Int, Maybe ExplodedPair) -> SFN -> (SFN, Int, Maybe ExplodedPair)
-- Short circuit once an explosion has been found.
doExplodePart1 (index, depth, explodedPair@(Just _)) sfn = (sfn, index, explodedPair)
-- Increment our index whenever we see a regular number.
doExplodePart1 (index, depth, Nothing) (Num n) = (Num n, index + 1, Nothing)
-- Explode any pair nested inside four other pairs. It will always be a pair of regular numbers.
doExplodePart1 (index, 4, Nothing) explodedPair@(Pair (Num _, Num _)) = (Num 0, index, Just (index, explodedPair))
doExplodePart1 (index, 4, Nothing) (Pair (left, right)) = error "Invalid Snailfish number"
-- Recurse through pairs.
doExplodePart1 (initialIndex, depth, Nothing) (Pair (left, right))
    =   let (newLeft, tempIndex, tempExplodedPair) = doExplodePart1 (initialIndex, depth + 1, Nothing) left
            (newRight, finalIndex, finalExplodedPair) = doExplodePart1 (tempIndex, depth + 1, tempExplodedPair) right
        in  (Pair (newLeft, newRight), finalIndex, finalExplodedPair)

-- Add the values from the exploded pair to the regular numbers immediately before and after.
doExplodePart2 :: Int -> ExplodedPair -> SFN -> (SFN, Int)
doExplodePart2 index (explosionIndex, Pair (Num explodedLeft, Num explodedRight)) (Num n)
    | index == explosionIndex - 1 = (Num (n + explodedLeft), index + 1)
    | index == explosionIndex + 1 = (Num (n + explodedRight), index + 1)
    | otherwise = (Num n, index + 1)
doExplodePart2 initialIndex explodedPair (Pair (left, right))
    =   let (newLeft, tempIndex) = doExplodePart2 initialIndex explodedPair left
            (newRight, finalIndex) = doExplodePart2 tempIndex explodedPair right
        in  (Pair (newLeft, newRight), finalIndex)

doExplode :: SFN -> (Bool, SFN)
doExplode sfn = maybeDoPart2 $ doExplodePart1 (0, 0, Nothing) sfn
    where maybeDoPart2 (_, _, Nothing) = (False, sfn)
          maybeDoPart2 (part1SFN, _, Just explodedPair) = (True, fst $ doExplodePart2 0 explodedPair part1SFN)

divRoundUp :: Int -> Int -> Int
divRoundUp value divisor
    | odd value = (value + 1) `div` divisor
    | otherwise = value `div` divisor

split :: Bool -> SFN -> (Bool, SFN)
split True sfn = (True, sfn) -- Short circuit once we've made a change. We only make one change at a time
split changed (Num n) = if n > 9 then (True, Pair (Num (n `div` 2), Num (n `divRoundUp` 2))) else (changed, Num n)
split initialChanged (Pair (left, right)) = let (tempChanged, newLeft) = split initialChanged left
                                                (finalChanged, newRight) = split tempChanged right
                                            in  (finalChanged, Pair (newLeft, newRight))

doSplit :: SFN -> (Bool, SFN)
doSplit = split False

-- Exploding takes priority over splitting. Keep reducing until there's no more changes.
reduce :: SFN -> SFN
reduce sfn =    let (exploded, tempSFN) = doExplode sfn
                    (changed, reducedSFN) = if not exploded then doSplit tempSFN else (True, tempSFN)
                in if not changed then sfn else reduce reducedSFN

-- Note that all Snailfish numbers must be reduced.
add :: SFN -> SFN -> SFN
add a b = reduce $ Pair (a, b)

magnitude :: SFN -> Int
magnitude (Num n) = n
magnitude (Pair (left, right)) = (3 * magnitude left) + (2 * magnitude right)