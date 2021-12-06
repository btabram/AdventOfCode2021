import qualified Data.Map as Map
import Data.List (foldl')
import Data.List.Split (splitOn) -- required `cabal install --lib split`

-- The important thing about a given fish is the number of days until it spawns, we can group equivalent fish together.
-- |Integer| has arbitrary precision so we don't need to worry about overflow.
data FishState = FishState { n0 :: Integer, n1 :: Integer, n2 :: Integer
                           , n3 :: Integer, n4 :: Integer, n5 :: Integer
                           , n6 :: Integer, n7 :: Integer, n8 :: Integer
                           } deriving (Show)

main = do
    input <- fmap lines $ readFile "input.txt"
    let initialFish = map read $ splitOn "," (head input) :: [Integer]
    let initialFishNumbers = foldl' (\counts fish -> Map.insertWith (+) fish 1 counts) Map.empty initialFish
    let getIFN n = Map.findWithDefault 0 n initialFishNumbers
    let initialFishState = FishState { n0 = getIFN 0, n1 = getIFN 1, n2 = getIFN 2
                                     , n3 = getIFN 3, n4 = getIFN 4, n5 = getIFN 5
                                     , n6 = getIFN 6, n7 = getIFN 7, n8 = getIFN 8 }
    putStrLn $ "The answer to Part 1 is: " ++ (show $ countFish $ advanceDays initialFishState 80)
    putStrLn $ "The answer to Part 2 is: " ++ (show $ countFish $ advanceDays initialFishState 256)

-- Day 0 fish spawn a new fish (which will spawn in 8 days) and reset their to-spawn time to 6 days.
advanceDays :: FishState -> Integer -> FishState
advanceDays state 0 = state
advanceDays FishState{ n0, n1, n2, n3, n4, n5, n6, n7, n8 } days
    = advanceDays (FishState n1 n2 n3 n4 n5 n6 (n7 + n0) n8 n0) (days - 1)

countFish :: FishState -> Integer
countFish FishState{ n0, n1, n2, n3, n4, n5, n6, n7, n8 } = n0 + n1 + n2 + n3 + n4 + n5 + n6 + n7 + n8