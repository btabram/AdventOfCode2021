import Data.Char (isLower)
import Data.List (foldl', nub)
import Data.List.Split (splitOn) -- required `cabal install --lib split`
import Data.Map ((!))
import qualified Data.Map as Map

main = do
    inputLines <- fmap lines $ readFile "input.txt"
    let connections = map (splitOn "-") inputLines
    let uniqueCaves = nub $ concat connections
    let cavesMap = Map.fromList [(c, getConnectedCaves c connections) | c <- uniqueCaves]
    putStrLn $ "The answer to Part 1 is " ++ (show $ length $ walkCaves cavesMap False ["start"] [])
    putStrLn $ "The answer to Part 2 is " ++ (show $ length $ walkCaves cavesMap True ["start"] [])

-- |connections| is a list of length two lists. Each inner list contains two caves that are connected
getConnectedCaves :: String -> [[String]] -> [String]
getConnectedCaves cave connections =    let relevantConnections = filter (any (==cave)) connections
                                        in nub $ filter (/=cave) $ concat relevantConnections

walkCaves :: Map.Map String [String] -> Bool -> [String] -> [[String]] -> [[String]]
walkCaves cavesMap part2 path completePaths
    | head path == "start" && length path > 1 = completePaths -- Can't go back to start. Discard path
    | isIllegalPath part2 path = completePaths -- Path is illegal. Discard it
    | head path == "end" = path:completePaths -- Finished. Add path to known paths
    -- Otherwise try walking to all neighbours
    | otherwise =   let walkNext = (\next completePaths -> walkCaves cavesMap part2 (next:path) completePaths)
                        walkConnections = map walkNext (cavesMap ! (head path))
                    in foldl' (\acc f -> f acc) completePaths walkConnections

-- In part 1 we can't visit any small caves more than once. In part 2 we can visit one small cave twice
isIllegalPath :: Bool -> [String] -> Bool
isIllegalPath part2 path =  let smallCaves = filter (any isLower) path
                                count = length smallCaves
                                countUnique = length $ nub smallCaves
                            in if part2 then count > countUnique + 1 else count > countUnique