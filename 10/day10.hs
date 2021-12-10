import Control.Monad (foldM)
import Data.Either (fromLeft, fromRight)
import Data.List (foldl', sort)

main = do
    inputLines <- fmap lines $ readFile "input.txt"
    let maybeCorrupt = map checkCorruption inputLines
    -- Sum up all the illegal character scores. Lines which aren't corrupt / `Left` score 0
    putStrLn $ "The answer to Part 1 is: " ++ (show $ foldl' (\acc x -> acc + fromLeft 0 x) 0 maybeCorrupt)
    -- After filtering out corrupt lines we have the unmatched chunks of incomplete lines
    let incompleteLineChunks = filter (not . null) $ map (fromRight []) maybeCorrupt
    let sortedAutocompleteScores = sort $ map scoreAutocomplete incompleteLineChunks
    let middleIndex = length sortedAutocompleteScores `div` 2
    putStrLn $ "The answer to Part 2 is: " ++ (show $ sortedAutocompleteScores !! middleIndex)

-- Return either the incomplete left hand side of the line or the score of the first illegal character
checkCorruption :: [Char] -> Either Int [Char]
checkCorruption line = foldM checkChunk [] line
    where   checkChunk ('(':stack) ')' = Right stack -- Next char completes (and removes) chunk on top of the stack
            checkChunk ('[':stack) ']' = Right stack
            checkChunk ('{':stack) '}' = Right stack
            checkChunk ('<':stack) '>' = Right stack
            checkChunk _ ')' = Left 3 -- Next char is illegal and fails to complete chunk on top of the stack
            checkChunk _ ']' = Left 57
            checkChunk _ '}' = Left 1197
            checkChunk _ '>' = Left 25137
            checkChunk stack c = Right (c:stack) -- Starting a new chunk. Add to stack

scoreAutocomplete :: [Char] -> Int
scoreAutocomplete incompleteChunks = foldl' (\scoreAcc char -> (scoreAcc * 5) + (scoreBracket char)) 0 incompleteChunks
    where scoreBracket '(' = 1
          scoreBracket '[' = 2
          scoreBracket '{' = 3
          scoreBracket '<' = 4