import Data.List (elemIndices, find, foldl', groupBy, minimumBy, permutations, sort)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set


type Pos = (Int, Int)
type AmphiType = Char
type AmphiPosition = (AmphiType, Pos, Bool)
type AmphiPositions = [AmphiPosition]


main = do
    part1Input <- fmap (trimInput . lines) $ readFile "input.txt"

    -- Part 2 has bigger rooms and adds some extra lines to the input.
    let (before, after) = splitAt 2 part1Input
    let part2Input = before ++ [" #D#C#B#A#", " #D#B#A#C#"] ++ after

    let inputToState input = concatMap (\t -> map (t, , False) (getPositions input t)) ['A', 'B', 'C', 'D']

    putStrLn $ "The answer to Part 1 is " ++ show (aStar (inputToState part1Input, 2, 0) Map.empty Set.empty)
    putStrLn $ "The answer to Part 1 is " ++ show (aStar (inputToState part2Input, 4, 0) Map.empty Set.empty)


-- We use a coordinate system where the origin is the leftmost space in the corridor:
--    0123456789 x
--   #############
-- 0 #...........#
-- 1 ###B#C#B#D###
-- 2   #A#D#C#A#
-- y   #########

-- Trim the input its indicies match up with our coordinate system.
trimInput :: [String] -> [String]
trimInput = map tail . tail

getPositions :: [String] -> Char -> [Pos]
getPositions trimmedInputLines typeChar =
    concatMap (\(xs, y) -> map (,y) xs) (zip (map (elemIndices typeChar) trimmedInputLines) [0..])


getType :: AmphiPosition -> AmphiType
getType (t, p, c) = t

getPos :: AmphiPosition -> Pos
getPos (t, p, c) = p

getCorrect :: AmphiPosition -> Bool
getCorrect (t, p, c) = c


isNotCorridor :: Pos -> Bool
isNotCorridor (x, y) = y /= 0

isOccupied :: AmphiPositions -> Pos -> Bool
isOccupied state p = p `elem` map getPos state

getOccupier :: AmphiPositions -> Pos -> Maybe AmphiType
getOccupier state p = getType <$> find ((== p) . getPos) state


getValidNeighbours :: AmphiPositions -> Int -> Pos -> [Pos]
getValidNeighbours state roomSize (x, y) = filter isOpenSpace [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    where   isOpenSpace (x, y)
                | isOccupied state (x, y)   = False
                | y == 0                    = x >= 0 && x <= 10 -- Corridor
                | y > 0 && y <= roomSize    = x `elem` [2, 4, 6, 8] -- Rooms
                | otherwise                 = False

floodFill :: AmphiPositions -> Int -> Pos -> Set.Set Pos -> Set.Set Pos
floodFill state roomSize (x, y) filled
    -- Do nothing if (x, y) is already filled.
    | Set.member (x, y) filled = filled
    -- Otherwise fill (x, y) and then try filling any potential moves from here.
    | otherwise =   let filledHere = Set.insert (x, y) filled
                        recurse = floodFill state roomSize
                    in  foldr recurse filledHere (getValidNeighbours state roomSize (x, y))

getMoves :: AmphiPositions -> Int -> AmphiPosition -> [Pos]
getMoves state roomSize (t, p, _) = filter allowed $ Set.toList $ Set.delete p $ floodFill state roomSize p Set.empty
    where   allowed (x, y) -- We're assuming here that (x, y) is a valid move (e.g. no negatives)
                -- Amphis can only end a move in the corridor if they started in a room. They can't stop outside rooms.
                | y == 0    = isNotCorridor p && (odd x || x == 0 || x == 10)
                -- Amphis will only move into a romm if it's a correct final position for them.
                | otherwise = isCorrect state roomSize t (x, y)


getRoomX :: AmphiType -> Int
getRoomX 'A' = 2
getRoomX 'B' = 4
getRoomX 'C' = 6
getRoomX 'D' = 8

isCorrect :: AmphiPositions -> Int -> AmphiType -> Pos -> Bool
isCorrect state roomSize t (x, y)
    -- Wrong room.
    | x /= getRoomX t   = False
    -- Right room and no need to move because we're at the end.
    | y == roomSize     = True
    -- Right room but we might need to move to let out amphis for whom this is the wrong room.
    | otherwise         = all (\yBelow -> getOccupier state (x, yBelow) == Just t) [(y + 1) .. roomSize]


-- The distance calculation isn't just Manhattan distance because we always go via the corridor (y = 0).
getEnergyCost :: AmphiPosition -> Pos -> Int
getEnergyCost (t, (x1, y1), _) (x2, y2) = getMultiplier t * (abs (x2 - x1) + y1 + y2)
    where   getMultiplier 'A' = 1
            getMultiplier 'B' = 10
            getMultiplier 'C' = 100
            getMultiplier 'D' = 1000


-- Note that a move into a room (rather than the corridor) must be a move into a correct position.
makeMove :: AmphiPositions -> AmphiPosition -> Pos -> AmphiPositions
makeMove (amphi : rest) amphiToMove newPos
    | amphi == amphiToMove  = (getType amphi, newPos, isNotCorridor newPos) : rest
    | otherwise             = amphi : makeMove rest amphiToMove newPos

-- Sort our state, within the different amphi types, because all amphis of a type are fungible and we don't want to
-- waste time investigating 'different' states that are actually equivalent.
updateState :: AmphiPositions -> AmphiPosition -> Pos -> AmphiPositions
updateState state amphi newPos =    let newState = makeMove state amphi newPos
                                        stateByType = groupBy (\a b -> getType a == getType b) newState
                                    in  concatMap sort stateByType


-- Estimate the cost to get from a given state to a solved state. This must never be more than the actual cost. We look
-- for the minimum cost to move all amphis into the correct positions, ignoring collisions.
hueristic:: AmphiPositions -> Int -> Int
hueristic [] _ = 0
hueristic state roomSize =
    let (amphisOfType, rest) = splitAt roomSize state
        roomX = getRoomX $ getType $ head amphisOfType
        roomPositions = [(roomX, y) | y <- [1 .. roomSize]]
        costPermutations = map (sum . map (uncurry getEnergyCost) . zip amphisOfType) $ permutations roomPositions
    in  minimum costPermutations + hueristic rest roomSize

aStar :: (AmphiPositions, Int, Int) -> Map.Map AmphiPositions Int -> Set.Set AmphiPositions -> Int
aStar (currentState, roomSize, currentCost) unvisited visited
    | all getCorrect currentState = currentCost -- Done when all amphis are in a correct position
    | otherwise =
        let needToMove = filter (not . getCorrect) currentState
            moves = concatMap (\amphi -> map (amphi,) (getMoves currentState roomSize amphi)) needToMove
            movesWithNewStates = map (\(amphi, p) -> (updateState currentState amphi p, amphi, p)) moves
            filtered = filter (\(s, _, _) -> Set.notMember s visited) movesWithNewStates
            -- Use a hueristic to prefer states which are closer to the final state.
            statesWithHCosts = map (\(s, a, p) -> (s, currentCost + getEnergyCost a p + hueristic s roomSize)) filtered
            -- Update our unvisited costs map. We might have found a cheaper path to an unvisited state.
            tempUnvisited = foldl' (\acc (state, cost) -> Map.insertWith min state cost acc) unvisited statesWithHCosts
            -- The next node to visit is the lowest cost unvisited node.
            (nextState, nextHeuristicCost) = minimumBy (comparing snd) $ Map.toList tempUnvisited
            nextCost = nextHeuristicCost - hueristic nextState roomSize
            -- Visit the next state.
            newUnvisited = Map.delete nextState tempUnvisited
            newVisited = Set.insert nextState visited
        in  aStar (nextState, roomSize, nextCost) newUnvisited newVisited