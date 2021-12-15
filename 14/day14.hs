import Data.Function (on)
import Data.List (foldl', foldl1', groupBy, sortBy)
import Data.List.Split (splitOn) -- required `cabal install --lib split`
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Ord (comparing)

type Pair = (Char, Char)
type PairInsertions = Map.Map Pair [Pair]

data PairCounts = PairCounts { pairs :: [(Pair, Integer)], start :: Char, end :: Char } deriving (Show)

main = do
    inputLines <- fmap lines $ readFile "input.txt"
    let ([template], _:pairInsertionStrings) = break null inputLines
    let pairInsertions = Map.fromList $ map parsePairInsertion pairInsertionStrings
    let initialPairs = [(pair, 1)  | pair <- zip template (tail template)]
    let initialPairCounts = combinePairs $ PairCounts initialPairs (head template) (last template)
    let iteratedPairCounts = iterate (doStep pairInsertions) initialPairCounts
    let part1CharCounts = getCharCount $ iteratedPairCounts !! 10
    putStrLn $ "The answer to Part 1 is " ++ (show (maximum part1CharCounts - minimum part1CharCounts))
    let part2CharCounts = getCharCount $ iteratedPairCounts !! 40
    putStrLn $ "The answer to Part 2 is " ++ (show (maximum part2CharCounts - minimum part2CharCounts))

-- Transform from "NH -> C" to (('N', 'H'), [('N', 'C'), ('C', 'H')])
parsePairInsertion :: String -> (Pair, [Pair])
parsePairInsertion string = let [[first, second], [result]] = splitOn " -> " string
                            in ((first, second), [(first, result), (result, second)])

doStep :: PairInsertions -> PairCounts -> PairCounts
doStep insertions = combinePairs . doPairInsertion insertions

doPairInsertion :: PairInsertions -> PairCounts -> PairCounts
doPairInsertion insertions (PairCounts pairs s e) = PairCounts (concat $ map doInserts pairs) s e
    where doInserts (pair, count) = map (\newPair -> (newPair, count)) (insertions ! pair)

combinePairs :: PairCounts -> PairCounts
combinePairs (PairCounts pairs s e) =   let sorted = sortBy (comparing fst) pairs
                                            grouped = groupBy ((==) `on` fst) sorted
                                            newPairs = map (foldl1' (\(pair, acc) (_, n) -> (pair, acc + n))) grouped
                                        in PairCounts newPairs s e

getCharCount :: PairCounts -> [Integer]
getCharCount (PairCounts pairs start end) = let count = countPairChars pairs
                                                -- The above double-counts all chars except the start and end
                                                countWithStartAndEnd = updateCount [(start, 1), (end, 1)] count
                                            in map (`div` 2) $ Map.elems countWithStartAndEnd

countPairChars :: [(Pair, Integer)] -> Map.Map Char Integer
countPairChars = foldl' (\acc ((c1, c2), n) -> updateCount [(c1, n), (c2, n)] acc) Map.empty

updateCount :: [(Char, Integer)] -> Map.Map Char Integer -> Map.Map Char Integer
updateCount updates countMap = foldl' (\acc (pair, n) -> Map.insertWith (+) pair n acc) countMap updates