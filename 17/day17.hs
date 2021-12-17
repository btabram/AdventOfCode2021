import Text.Regex.TDFA ((=~)) -- required `cabal install --lib regex-tdfa`

main = do
    input <- fmap (head . lines) $ readFile "input.txt"
    let target@((xMin, xMax), (yMin, yMax)) = getTargetBounds input

    -- Anyhing larger than |xMax| will overshoot the target in a single step
    let xVelocities = [0..xMax]

    -- When we shoot the probe upwards with velocity v it will always come back down to the starting height with
    -- velocity -v because of the constant acceleration. So any initial upwards velocity greater than |yMin| will
    -- overshoot the target the turn after it returns to y == 0. For the lower bound we know anything less than |yMin|
    -- will always undershoot the target in a single step
    let yVelocities = [yMin..(-yMin)]

    let possibleInitialVelocities = [(x, y) | x <- xVelocities, y <- yVelocities]
    let successfulInitialVelocities = filter (probeHitsTarget target) possibleInitialVelocities

    let part1 = getMaxHeight $ maximum $ map snd successfulInitialVelocities
    let part2 = length successfulInitialVelocities
    putStrLn $ "The answer to Part 1 is " ++ (show part1)
    putStrLn $ "The answer to Part 2 is " ++ (show part2)

inputRegex = "^target area: x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)$"

getTargetBounds :: String -> ((Int, Int), (Int, Int))
getTargetBounds str =   let (_, _, _, groups) = str =~ inputRegex :: (String, String, String, [String])
                            [xMin, xMax, yMin, yMax] = map read groups :: [Int]
                        in ((xMin, xMax), (yMin, yMax))

probeHitsTarget :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
probeHitsTarget target initialVelocity = impl target (0, 0) initialVelocity
    where impl target@((xMin, xMax), (yMin, yMax)) (x, y) (vX, vY)
            | x > xMax || y < yMin = False
            | x >= xMin && x <= xMax && y >= yMin && y <= yMax = True
            | otherwise = impl target (x + vX, y + vY) (max (vX - 1) 0, vY - 1)

getMaxHeight :: Int -> Int
getMaxHeight = impl 0 0
    where impl maxHeight y vY
            | vY < 0 = maxHeight
            | otherwise = impl (max y maxHeight) (y + vY) (vY - 1)