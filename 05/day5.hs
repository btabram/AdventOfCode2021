import Data.List (foldl', group, sort)
import Text.Regex.TDFA ((=~)) -- required `cabal install --lib regex-tdfa`

type Point = (Int,Int)

data Vent = Vent { x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int } deriving (Show)

main = do
    lines <- fmap lines $ readFile "input.txt"
    let vents = map parseVent lines
    let part1 = countVentOverlaps $ filter isAxial vents
    let part2 = countVentOverlaps vents
    putStrLn $ "The answer to Part 1 is: " ++ (show part1)
    putStrLn $ "The answer to Part 2 is: " ++ (show part2)

inputRegex = "^([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)$"

parseVent :: String -> Vent
parseVent line =    let (_, _, _, groups) = line =~ inputRegex :: (String, String, String, [String])
                        [x1, y1, x2, y2] = map read groups :: [Int]
                    in Vent x1 y1 x2 y2

isHorizontal :: Vent -> Bool
isHorizontal Vent{ x1, y1, x2, y2 } = x1 == x2

isVertical :: Vent -> Bool
isVertical Vent{ x1, y1, x2, y2 } = y1 == y2

isAxial :: Vent -> Bool
isAxial v = isHorizontal v || isVertical v

pointsCovered :: Vent -> [Point]
pointsCovered Vent{ x1, y1, x2, y2 } =  let xStep = signum (x2 - x1)
                                            yStep = signum (y2 - y1)
                                        in fillLine (x1, y1) xStep yStep (x2, y2)

-- All lines are either horizontal, vertical, or at exactly 45 degrees.
fillLine :: Point -> Int -> Int -> Point -> [Point]
fillLine (x, y) xStep yStep finish@(xFinish, yFinish)
    | x == xFinish && y == yFinish  = [(x, y)]
    | otherwise                     = (x, y) : fillLine (x + xStep, y + yStep) xStep yStep finish

countVentOverlaps :: [Vent] -> Int
countVentOverlaps vents =   let allVentPoints = foldl' (\acc vent -> (pointsCovered vent) ++ acc) [] vents
                                groupedPoints = group $ sort allVentPoints
                                pointCounts = map length groupedPoints
                            in length $ filter (>1) pointCounts