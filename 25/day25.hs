import qualified Data.Set as Set


data Bounds = Bounds { xMax :: Int, yMax :: Int } deriving (Show)
type Pos = (Int, Int)
type State = (Set.Set Pos, Set.Set Pos)


main = do
    input <- fmap lines $ readFile "input.txt"
    let bounds = Bounds (length (head input)) (length input)
    let enumeratedInput = zip [0..] $ map (zip [0..]) input

    let eastCucumbers = populateSet enumeratedInput '>'
    let southCucumbers = populateSet enumeratedInput 'v'

    let getNextEast = getNextPos bounds '>'
    let getNextSouth = getNextPos bounds 'v'

    let stepsTillSteadyState = moveCucumbers (getNextEast, getNextSouth) (eastCucumbers, southCucumbers) 0
    putStrLn $ "The answer to Part 1 is " ++ show stepsTillSteadyState


populateSet :: [(Int, [(Int, Char)])] -> Char -> Set.Set Pos
populateSet grid cucumber =
    foldr (\(y, line) s -> foldr (\(x, c) s -> if c == cucumber then Set.insert (x, y) s else s) s line) Set.empty grid


getNextPos :: Bounds -> Char -> Pos -> Pos
getNextPos bounds '>' (x, y) = ((x + 1) `mod` xMax bounds, y)
getNextPos bounds 'v' (x, y) = (x, (y + 1) `mod` yMax bounds)


canMove :: (Pos -> Pos) -> State -> Pos -> Bool
canMove getNext (easts, souths) pos =   let nextPos = getNext pos
                                        in  not (Set.member nextPos easts || Set.member nextPos souths)

getMovers :: (Pos -> Pos) -> State -> Set.Set Pos -> [Pos]
getMovers getNext s positions = filter (canMove getNext s) $ Set.toList positions

move :: (Pos -> Pos) -> State -> Set.Set Pos -> [Pos] -> Set.Set Pos
move getNext s positions toMove =   let tempPositions = foldr Set.delete positions toMove
                                    in  foldr (Set.insert . getNext) tempPositions toMove


moveCucumbers :: (Pos -> Pos, Pos -> Pos) -> State -> Int -> Int
moveCucumbers getters@(getNextEast, getNextSouth) s@(positionsEast, positionsSouth) prevStep =
    let eastMovers = getMovers getNextEast s positionsEast
        newPositionsEast = move getNextEast s positionsEast eastMovers
        tempS = (newPositionsEast, positionsSouth)
        southMovers = getMovers getNextSouth tempS positionsSouth
        newPositionsSouth = move getNextSouth tempS positionsSouth southMovers
        step = prevStep + 1
        finished = length eastMovers == 0 && length southMovers == 0
    in  if finished then step else moveCucumbers getters (newPositionsEast, newPositionsSouth) step