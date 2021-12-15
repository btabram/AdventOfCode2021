import Data.Array ((!))
import qualified Data.Array as Array
import Data.Char (digitToInt)
import Data.List (foldl', minimumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

type Node = (Int, Int)
type Weights = Array.Array Node Int
type CostsMap = Map.Map Node Int

main = do
    inputLines <- fmap lines $ readFile "input.txt"
    let size = length inputLines -- Assume square input
    let grid = Array.listArray ((0, 0), (size-1, size-1)) (map digitToInt $ concat inputLines)
    let start = ((0, 0), 0) -- Node and cost

    let part1Destination = (size-1, size-1)
    let part1NeighbourFn = getNeighbourWeights grid (Array.bounds grid) (!)
    let part1MinCosts = dijkstra part1NeighbourFn part1Destination start Map.empty Map.empty
    putStrLn $ "The answer to Part 1 is " ++ (show $ (Map.!) part1MinCosts part1Destination)

    let part2Destination = (5*size - 1, 5*size - 1)
    let part2NeighbourFn = getNeighbourWeights grid ((0, 0), part2Destination) (part2Lookup size)
    let part2MinCosts = dijkstra part2NeighbourFn part2Destination start Map.empty Map.empty
    putStrLn $ "The answer to Part 2 is " ++ (show $ (Map.!) part2MinCosts part2Destination)

dijkstra :: (Node -> [(Node, Int)]) -> Node -> (Node, Int) -> CostsMap -> CostsMap -> CostsMap
dijkstra neighbourFn destNode (currentNode, currentCost) unvisited visited
    | currentNode == destNode = visited
    | otherwise =   let neighbourWeights = filter (\(n, _) -> Map.notMember n visited) $ neighbourFn currentNode
                        neighbourCosts = map (\(n, w) -> (n, currentCost + w)) neighbourWeights
                        -- Update our unvisited costs map. We might have found a cheaper path to an unvisited node
                        tempUnvisited = foldl' (\acc (n, c) -> Map.insertWith min n c acc) unvisited neighbourCosts
                        -- The next node to visit is the lowest cost unvisited node
                        next@(nextNode, nextCost) = minimumBy (comparing snd) $ Map.toList tempUnvisited
                        -- Visit the next node
                        newUnvisited = Map.delete nextNode tempUnvisited
                        newVisited = Map.insert nextNode nextCost visited
                    in dijkstra neighbourFn destNode next newUnvisited newVisited

getNeighbourWeights :: Weights -> (Node, Node) -> (Weights -> Node -> Int) -> Node -> [(Node, Int)]
getNeighbourWeights weights bounds lookupFn (x, y) =    let neighbours = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
                                                            validNeighbours = filter (Array.inRange bounds) neighbours
                                                        in map (\n -> (n, lookupFn weights n)) validNeighbours

part2Lookup :: Int -> Weights -> Node -> Int
part2Lookup size weights (x, y) =   let manhattanDistanceFromOriginalTile = (x `div` size) + (y `div` size)
                                        originalTileValue = weights ! (x `mod` size, y `mod` size)
                                    in wrapAround9 (originalTileValue + manhattanDistanceFromOriginalTile)

wrapAround9 :: Int -> Int
wrapAround9 value
    | value <= 9 = value
    | otherwise = wrapAround9 (value - 9)