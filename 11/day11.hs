import Data.Char (digitToInt)
import Data.List (foldl')
import qualified Data.Map as Map

type Point = (Int, Int)
type OctopusEnergies = Map.Map Point Int

main = do
    input <- fmap (filter $ not . (=='\n')) $ readFile "input.txt"
    -- Map of position (x,y) to energy level. We know the input is a 10 x 10 grid.
    let initialEnergies = Map.fromList [((i `mod` 10, i `div` 10), digitToInt c) | (i, c) <- zip [0..] input]
    putStrLn $ "The answer to Part 1 is " ++ (show $ part1 initialEnergies)
    putStrLn $ "The answer to Part 2 is " ++ (show $ part2 initialEnergies)

part1 :: OctopusEnergies -> Int
part1 = countFlashes 100
    where   countFlashes 0 _ = 0
            countFlashes n energies =   let (nextEnergies, flashes) = doStep energies
                                        in flashes + countFlashes (n - 1) nextEnergies

part2 :: OctopusEnergies -> Int
part2 = getAllFlashStep 1
    where getAllFlashStep n energies =  let (nextEnergies, flashes) = doStep energies
                                        in if flashes == 100 then n else getAllFlashStep (n + 1) nextEnergies

doStep :: OctopusEnergies -> (OctopusEnergies, Int)
doStep energies =   let nextEnergies = resolveAllFlashes $ fmap (+1) energies
                    in (nextEnergies, length $ Map.filter (\e -> e == 0) nextEnergies)

resolveAllFlashes :: OctopusEnergies -> OctopusEnergies
resolveAllFlashes energies
    | any (>9) energies =   let flashPoints = Map.foldrWithKey (\k v acc -> if v > 9 then k:acc else acc) [] energies
                            in resolveAllFlashes $ foldl' doFlash energies flashPoints
    | otherwise = energies

doFlash :: OctopusEnergies -> Point -> OctopusEnergies
doFlash energies flashPoint = (incrementNeighbours . zeroSelf) energies
    where   zeroSelf = Map.adjust (\_ -> 0) flashPoint
            incrementNeighbours = (flip $ foldl' incrementEnergy) (getNeighbours flashPoint)

incrementEnergy :: OctopusEnergies -> Point -> OctopusEnergies
incrementEnergy = flip $ Map.adjust (\v -> if v == 0 then v else v + 1)

getNeighbours :: Point -> [Point]
getNeighbours self@(x, y) = [(x_n, y_n) | x_n <- [x-1 .. x+1], y_n <- [y-1 .. y+1], (x_n, y_n) /= self]