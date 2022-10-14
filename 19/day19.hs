import Data.Maybe (isJust)
import Data.List (foldl', transpose)
import Data.List.Split (splitOn) -- required `cabal install --lib split`
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~)) -- required `cabal install --lib regex-tdfa`


type Coord = (Int, Int, Int)
data Scanner = Scanner { num :: Int, beacons :: [Coord], pos :: Coord } deriving (Show)


main = do
    input <- fmap lines $ readFile "input.txt"
    let scanners = map toScanner $ splitOn [""] input
    let solvedScanners = solve [head scanners] (tail scanners)
    let part1 = Set.size $ foldr (\(Scanner _ beacons _) acc -> foldr Set.insert acc beacons) Set.empty solvedScanners
    let scannerPositions = map pos solvedScanners
    let part2 = maximum [manhattanDistance i j | i <- scannerPositions, j <- scannerPositions]
    putStrLn $ "The answer to Part 1 is " ++ show part1
    putStrLn $ "The answer to Part 2 is " ++ show part2


scannerRegex = "^--- scanner ([0-9]+) ---$"

getScannerNumber :: String -> Int
getScannerNumber str =  let (_, _, _, groups) = str =~ scannerRegex :: (String, String, String, [String])
                        in  read $ head groups

parseCoord :: String -> Coord
parseCoord str = let [x, y, z] = splitOn "," str in (read x, read y, read z)

toScanner :: [String] -> Scanner
toScanner lines =   let number = getScannerNumber $ head lines
                    in  Scanner number (map parseCoord $ tail lines) (if number == 0 then (0, 0, 0) else (-1, -1, -1))


manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs  (y2 - y1) + abs (z2 - z1)


-- Had to do spend while rotating my fingers around in a right-hand rule position to figure these rotations out.
coordVariants :: Coord -> [Coord]
coordVariants = concatMap rotationsAboutFacingDirection . facingDirections
    where   facingDirections (x, y, z) = [(x, y, z), (-x, y, -z), (y, -x, z), (-y, -x, -z), (z, y, -x), (-z, y, x)]
            rotationsAboutFacingDirection (x, y, z) = [(x, y, z), (x, z, -y), (x, -y, -z), (x, -z, y)]

scannerVariants :: Scanner -> [Scanner]
scannerVariants (Scanner n beacons p) = [Scanner n newBeacons p | newBeacons <- transpose $ map coordVariants beacons]


-- Set a position for our scanner such that one of our beacon positions matches a beacon position from another scanner.
-- If the two scanners overlap and we've chosen one of the 12+ overlapping beacons then this will be the right position.
tryPosition :: Scanner -> Coord -> Coord -> Scanner
tryPosition (Scanner num beacons _) (ourX, ourY, ourZ)  (otherX, otherY, otherZ)=
    let dx = otherX - ourX
        dy = otherY - ourY
        dz = otherZ - ourZ
    in  Scanner num (map (\(x, y, z) -> (x + dx, y + dy, z + dz)) beacons) (dx, dy, dz)

checkOverlap :: Scanner -> Scanner -> Maybe Scanner
checkOverlap a b =
    if Set.size (Set.intersection (Set.fromList $ beacons a) (Set.fromList $ beacons b)) >= 12 then Just b else Nothing

trySetScannerPosition :: Scanner -> Scanner -> Maybe Scanner
trySetScannerPosition a b = foldl' (\acc bv -> if isJust acc then acc else doForVariant bv) Nothing (scannerVariants b)
    where doForVariant i = let beaconCombinations = [(iBeacon, aBeacon) | iBeacon <- beacons i, aBeacon <- beacons a]
                               isToTry = map (uncurry (tryPosition i)) beaconCombinations
                           in  foldl' (\acc iToTry -> if isJust acc then acc else checkOverlap a iToTry) Nothing isToTry

solve :: [Scanner] -> [Scanner] -> [Scanner]
solve solved [] = solved
solve solved (toSolve : unsolvedRest) =
    let solution = foldl' (\acc s -> if isJust acc then acc else trySetScannerPosition s toSolve) Nothing solved
    in  case solution of   Just newSolved -> solve (newSolved : solved) unsolvedRest
                           Nothing -> solve solved (unsolvedRest ++ [toSolve]) -- Try again later if we couldn't solve