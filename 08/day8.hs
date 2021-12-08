import Data.List (delete, groupBy, sort, sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

main = do
    inputLines <- fmap lines $ readFile "input.txt"
    let parsedInput = map parseIntputLine inputLines
    let outputs = map snd parsedInput
    let part1 = sum $ map (length . filter (\x -> let l = length x in l == 2 || l == 3 || l == 4 || l == 7)) outputs
    putStrLn $ "The anwser to Part 1 is: " ++ (show part1)
    let part2 = sum $ map getOutputValue parsedInput
    putStrLn $ "The anwser to Part 2 is: " ++ (show part2)

parseIntputLine :: String -> ([String], [String])
parseIntputLine line =  let (uniqueSignalPatterns, separator:outputs) = break (=='|') line
                        in (words uniqueSignalPatterns, words outputs)

{-
We're using a seven-segment display with the segments and digits like so:
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
 -}
getSignalDigitMapping :: [String] -> Map.Map String Char -- The input is the 10 unique signal patterns (USP)
getSignalDigitMapping usp = let lengthSortedUSP = sortBy (\a b -> length a `compare` length b) usp
                                lengthGroupedUSP = groupBy (\a b -> length a == length b) lengthSortedUSP
                                -- |one| is the set of signal wires that form the 1 digit etc.
                                -- We can't distinguish the 0, 6 & 9 digit signal sets so group them in a list
                                [[one], [seven], [four], _, zeroSixNine, _] = map (map Set.fromList) lengthGroupedUSP
                                -- The signal wire for segment a is the only signal wire in 7 which isn't in 1
                                [a] = Set.toList $ Set.difference seven one
                                -- 6 is the only digit out of 0, 6 & 9 which don't include both signal wires in 1
                                [six] = filter (\x -> not $ one `Set.isSubsetOf` x) zeroSixNine
                                -- and so on...
                                [c] = Set.toList $ Set.difference one six
                                [f] = Set.toList $ Set.delete c one
                                bAndD = Set.difference four one
                                [zero] = filter (\x -> not $ bAndD `Set.isSubsetOf` x) zeroSixNine
                                [d] = Set.toList $ Set.difference bAndD zero
                                [b] = Set.toList $ Set.delete d bAndD
                                [nine] = delete zero $ delete six $ zeroSixNine
                                [e] = Set.toList $ Set.difference zero nine
                                [g] = Set.toList $ Set.difference nine (Set.union four seven)
                            -- Return a map of sorted lists of signal wires to digits
                            in Map.fromList [   (sort [a,b,c,e,f,g], '0'), (sort [c,f], '1'),
                                                (sort [a,c,d,e,g], '2'), (sort [a,c,d,f,g], '3'),
                                                (sort [b,c,d,f], '4'), (sort [a,b,d,f,g], '5'),
                                                (sort [a,b,d,e,f,g], '6'), (sort [a,c,f], '7'),
                                                (sort [a,b,c,d,e,f,g], '8'), (sort [a,b,c,d,f,g], '9')  ]

getOutputValue :: ([String], [String]) -> Int
getOutputValue (usp, output) =  let sigDigMap = getSignalDigitMapping usp
                                -- Use sorted keys in the map to cope with random signal wire orders.
                                in read $ map (\s -> fromJust $ Map.lookup (sort s) sigDigMap) output