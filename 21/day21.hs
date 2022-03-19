import Data.List (foldl', group, sort)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)


data PlayerState = PlayerState { position :: Int, score :: Int } deriving (Eq, Ord, Show)
type GameState = (Bool, PlayerState, PlayerState) -- (is it player 1's turn?, player 1 state, player 2 state)

type WinCounts = (Integer, Integer) -- (number of games won by player 1, number of games won by player 2)
type GameResultsCache = Map.Map GameState WinCounts


main = do
    input <- fmap lines $ readFile "input.txt"
    let [player1StartPos, player2StartPos] = map (read . last . words) input :: [Int]
    let initialGameState = (True, PlayerState player1StartPos 0, PlayerState player2StartPos 0)

    let ((_, player1State, player2State), _, totalRolls) = playPracticeGame (initialGameState, 0, 0)
    let losersScore = min (score player1State) (score player2State)
    putStrLn $ "The answer to Part 1 is " ++ (show $ losersScore * totalRolls)

    let (player1Wins, player2Wins) = snd $ playGame Map.empty initialGameState
    putStrLn $ "The answer to Part 2 is " ++ (show $ max player1Wins player2Wins)


-- Roll 3 deterministic dice and return (sum of the 3 dice, value of the most recent roll).
rollDeterministicDice :: Int -> (Int, Int)
rollDeterministicDice prevRoll = foldl' (\(acc, prev) _ -> let val = roll prev in (acc + val, val)) (0, prevRoll) [1..3]
    where   roll 100 = 1
            roll prevRoll = prevRoll + 1

movePlayer :: Int -> PlayerState -> PlayerState
movePlayer diceSum (PlayerState position score) = PlayerState newPosition newScore
    where   newPosition = ((position - 1 + diceSum) `mod` 10) + 1 -- Possible positions are 1-10
            newScore = score + newPosition

move :: Int -> GameState -> GameState
move diceSum (True, player1State, p2State) = (False, movePlayer diceSum player1State, p2State)
move diceSum (False, player1State, p2State) = (True, player1State, movePlayer diceSum p2State)

takePracticeTurn :: (GameState, Int, Int) -> (GameState, Int, Int)
takePracticeTurn (gameState, prevRoll, totalRolls) = (newGameState, newPrevRoll, totalRolls + 3)
    where   (diceSum, newPrevRoll) = rollDeterministicDice prevRoll
            newGameState = move diceSum gameState

playPracticeGame :: (GameState, Int, Int) -> (GameState, Int, Int)
playPracticeGame state =    let newState@((_, player1, player2), _, _) = takePracticeTurn state
                                finished = score player1 >= 1000 || score player2 >= 1000
                            in  if finished then newState else playPracticeGame newState

rollDiracDice :: [(Int, Integer)] -- [(sum of the 3 dice, number of universes with that sum)]
rollDiracDice = map (\a -> (a !! 0, toInteger $ length a)) $ group $ sort $ allPossibleRolls
    where allPossibleRolls = [i + j + k | i <- [1..3], j <- [1..3], k <- [1..3]]

-- Each roll of the Dirac dice splits the universe into multiple copies. Returns [(universe state, number of copies)].
takeQuantumTurn :: GameState -> [(GameState, Integer)]
takeQuantumTurn state = map (\(diceSum, occur) -> (move diceSum state, occur)) rollDiracDice

resolveNewUniverses :: (GameResultsCache, WinCounts) -> (GameState, Integer) -> (GameResultsCache, WinCounts)
resolveNewUniverses (cache, (initialPlayer1Wins, initialPlayer2Wins)) (state, copies) = (finalCache, totalWinCounts)
    where   (tempCache, newWinCounts@(newPlayer1Wins, newPlayer2Wins)) = playGame cache state
            finalCache = Map.insert state newWinCounts tempCache
            totalWinCounts = (initialPlayer1Wins + newPlayer1Wins*copies, initialPlayer2Wins + newPlayer2Wins*copies)

-- There's an enormous number of games with the Dirac dice but only 10s of thousands of unique game states so can save
-- a lot of computation with memoisation.
playGame :: GameResultsCache -> GameState -> (GameResultsCache, WinCounts)
playGame cache gameState =  if isJust cachedResults then (cache, fromJust cachedResults)
                            else if score player1 >= 21 then (Map.insert gameState player1Won cache, player1Won)
                            else if score player2 >= 21 then (Map.insert gameState player2Won cache, player2Won)
                            else foldl' resolveNewUniverses (cache, (0, 0)) allPossibleNewGameStates
    where   cachedResults = Map.lookup gameState cache
            (_, player1, player2) = gameState
            player1Won = (1, 0) :: WinCounts
            player2Won = (0, 1) :: WinCounts
            allPossibleNewGameStates = takeQuantumTurn gameState