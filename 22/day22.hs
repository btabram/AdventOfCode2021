import Data.List (foldl')
import Text.Regex.TDFA ((=~)) -- required `cabal install --lib regex-tdfa`


data Cuboid = Cuboid { xMin :: Int, xMax :: Int, yMin :: Int, yMax :: Int, zMin :: Int, zMax :: Int } deriving (Show)
data Region = Region { on :: Bool, whole :: Cuboid, holes :: [Region] } deriving (Show)

type RebootStep = (Bool, Cuboid)


main = do
    input <- fmap lines $ readFile "input.txt"
    let rebootSteps = map parseRebootStep input
    let cubesAfterSteps = sum . map countCubes . foldl' doRebootStep []
    putStrLn $ "The answer to Part 1 is " ++ show (cubesAfterSteps $ filter isInitStep rebootSteps)
    putStrLn $ "The answer to Part 2 is " ++ show (cubesAfterSteps rebootSteps)


inputRegex = "^(on|off) x=(-?[0-9]+)..(-?[0-9]+),y=(-?[0-9]+)..(-?[0-9]+),z=(-?[0-9]+)..(-?[0-9]+)$"

parseRebootStep :: String -> RebootStep
parseRebootStep str = let (_, _, _, groups) = str =~ inputRegex :: (String, String, String, [String])
                          [xMin, xMax, yMin, yMax, zMin, zMax] = map read $ tail groups :: [Int]
                      in  (head groups == "on", Cuboid xMin xMax yMin yMax zMin zMax)

isInitStep :: RebootStep -> Bool
isInitStep (on, Cuboid a b c d e f) = all (\x -> x >= -50 && x <= 50) [a, b, c, d, e, f]


-- To do a reboot step we first cut the cuboid from this step out of the regions of cubes which are currently on. If
-- this step wants to turn cubes off then we have nothing to do. If the step wants to turn cubes on them we add the
-- cuboid from this step to the list of on regions.
doRebootStep :: [Region] -> RebootStep -> [Region]
doRebootStep onRegions (on, stepCuboid)
    = let onRegionsWithoutStepCuboid = map (cutOutIntersection stepCuboid) onRegions
      in if on then Region True stepCuboid [] : onRegionsWithoutStepCuboid else onRegionsWithoutStepCuboid

-- Cut a cuboid out of our region, creating a new hole. We must cut the new hole out of the existing holes so that none
-- of our regions overlap (and can be counted later).
cutOutIntersection :: Cuboid -> Region -> Region
cutOutIntersection a (Region on b holes)
    = case getIntersection a b
      of Just intersection -> Region on b (Region (not on) intersection [] : map (cutOutIntersection a) holes)
         Nothing -> Region on b holes

getIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
getIntersection (Cuboid xMin_a xMax_a yMin_a yMax_a zMin_a zMax_a) (Cuboid xMin_b xMax_b yMin_b yMax_b zMin_b zMax_b)
    = let xMin_i = max xMin_a xMin_b -- Work out the bounds of a potential intersection (which will be cuboid)
          xMax_i = min xMax_a xMax_b
          yMin_i = max yMin_a yMin_b
          yMax_i = min yMax_a yMax_b
          zMin_i = max zMin_a zMin_b
          zMax_i = min zMax_a zMax_b
          isValidIntersection = xMin_i <= xMax_i && yMin_i <= yMax_i && zMin_i <= zMax_i
      in if isValidIntersection then Just (Cuboid xMin_i xMax_i yMin_i yMax_i zMin_i zMax_i) else Nothing

countCubes :: Region -> Int
countCubes (Region _ whole holes) = cuboidCubes whole - sum (map countCubes holes)
    where cuboidCubes (Cuboid xMin xMax yMin yMax zMin zMax) = (xMax - xMin + 1) * (yMax - yMin + 1) * (zMax - zMin + 1)