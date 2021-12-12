import Data.Array ((!))
import qualified Data.Array as Array
import Data.Char (digitToInt)
import Data.List (foldl', sort)
import qualified Data.Set as Set

main = do
    inputLines <- fmap lines $ readFile "input.txt"
    let height = length inputLines
    let width = length $ head inputLines
    let inputDigitList = map digitToInt $ concat inputLines
    let grid = Array.listArray ((1,1),(height,width)) inputDigitList
    let lowPoints = filter (\(pos,val) -> isLowPoint grid pos val) (Array.assocs grid)

    let part1 = foldl' (\acc (_,val) -> acc + val + 1) 0 lowPoints
    putStrLn $ "The answer to Part 1 is " ++ (show part1)

    let basins = map (\(pos,_) -> floodFill grid pos Set.empty) lowPoints
    let basinSizes = map Set.size basins
    let part2 = product $ take 3 $ reverse $ sort basinSizes
    putStrLn $ "The answer to Part 2 is " ++ (show part2)

floodFill :: Array.Array (Int,Int) Int -> (Int,Int) -> Set.Set (Int,Int) -> Set.Set (Int,Int)
floodFill grid (i,j) filled
    -- Do nothing if (i,j) is already filled, not in the grid, or is max height
    | Set.member (i,j) filled = filled
    | not (Array.inRange (Array.bounds grid) (i,j)) = filled
    | grid ! (i,j) == 9 = filled
    -- Otherwise fill (i,j) and then try filling the four neighbouring grid elements
    | otherwise =   let filledHere = Set.insert (i,j) filled
                        recurse = floodFill grid
                        fillNeighbours = recurse (i+1,j) . recurse (i-1,j) . recurse (i,j+1) . recurse (i,j-1)
                    in fillNeighbours filledHere

isLowPoint :: Array.Array (Int,Int) Int -> (Int,Int) -> Int -> Bool
isLowPoint grid (i,j) height =  let neighbours = [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
                                    validNeighbours = filter (Array.inRange (Array.bounds grid)) neighbours
                                    neighbourHeights = map (grid !) validNeighbours
                                in height < minimum neighbourHeights