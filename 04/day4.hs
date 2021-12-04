import Data.List (findIndex, groupBy, transpose)
import Data.List.Split (splitOn) -- required `cabal install --lib split`

-- Every entry on our bingo cards is either marked or unmarked.
data CardEntry = Marked Int | Unmarked Int deriving (Show)
type Card = [[CardEntry]]

main = do
    lines <- fmap lines $ readFile "input.txt"
    let drawNumbersString:cardsLines = lines
    let drawNumbers = map read $ splitOn "," drawNumbersString :: [Int]
    -- After grouping we have a list of lists. Each sublist is either an empty line or all the lines for a single card.
    let cardsStrings = filter (\sublist -> sublist /= [""]) $ groupBy (\a b -> null a == null b) cardsLines
    -- Map from [[String]] -> [[[Marked]]] by splitting the strings into words, reading the words as ints, and then
    -- creating an Unmarked CardEntry.
    let cards = map (map ((map (Unmarked . read)) . words)) cardsStrings
    let (p1BingoBoard, p1FinalNumber) = p1 cards drawNumbers
    let (p2BingoBoard, p2FinalNumber) = p2 cards drawNumbers
    putStrLn $ "The anwser to Part 1 is: " ++ show (scoreCard p1BingoBoard p1FinalNumber)
    putStrLn $ "The anwser to Part 2 is: " ++ show (scoreCard p2BingoBoard p2FinalNumber)

markCard :: Int -> Card -> Card
markCard drawnNumber card = map (map markNumber) card
    where   markNumber (Marked x) = Marked x
            markNumber (Unmarked x) = if x == drawnNumber then Marked x else Unmarked x

isMarked :: CardEntry -> Bool
isMarked (Marked _) = True
isMarked (Unmarked _) = False

isBingo :: Card -> Bool
isBingo card =  let rowsBingo = or $ map (\line -> all isMarked line) card
                    colsBingo = or $ map (\line -> all isMarked line) (transpose card)
                in rowsBingo || colsBingo

-- Part 1: return the first board to reach bingo and the number that was drawn to complete it.
p1 :: [Card] -> [Int] -> (Card, Int)
p1 cards (n:numbers) =  let markedCards = map (markCard n) cards
                        in case findIndex isBingo markedCards of    Just bingoIdx -> (markedCards !! bingoIdx, n)
                                                                    Nothing -> p1 markedCards numbers

-- Part 2: return the final board to reach bingo and the number that was drawn to complete it.
p2 :: [Card] -> [Int] -> (Card, Int)
p2 cards (n:numbers) =  let markedCards = map (markCard n) cards
                        in  if all isBingo markedCards
                            -- If every card is now at bingo then we're interested in the card which wasn't bingo until
                            -- the most recent number was drawn and marked. Assume there's exactly one such card.
                            then case findIndex (not . isBingo) cards of Just lastBiIdx -> (markedCards !! lastBiIdx, n)
                            else p2 markedCards numbers

-- Map all the entries to scores and then sum them all up.
scoreCard :: Card -> Int -> Int
scoreCard card finalNumber = (sum $ map sum (map (map scoreEntry) card)) * finalNumber
    where   scoreEntry (Marked _) = 0
            scoreEntry (Unmarked x) = x