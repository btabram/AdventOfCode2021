import Data.List (nub)
import Data.List.Split (splitOn) -- required `cabal install --lib split`
import qualified Data.Set as Set

newtype Dots = Dots { getList :: [(Int,Int)] }

instance Show (Dots) where
    show (Dots l) = let dotsSet = Set.fromList l
                        xRange = [0 .. maximum (map fst l)]
                        yRange = [0 .. maximum (map snd l)]
                    in concat [[if (x,y) `Set.member` dotsSet then '#' else '.' | x <- xRange] ++ "\n" | y <- yRange]

type Folds = [(Char,Int)]

main = do
    inputLines <- fmap lines $ readFile "input.txt"
    let (dotStrings, _:foldStrings) = break null inputLines
    -- Map from "2,3" to (2,3)
    let dots = Dots $ map (\dotStr -> let [x,y] = map read (splitOn "," dotStr) in (x,y)) dotStrings
    -- Map from "fold along x=5" to ('x',5)
    let folds = map (\foldStr -> let [rest,val] = splitOn "=" foldStr in (last rest, read val)) foldStrings :: Folds
    putStrLn $ "The answer to Part 1 is " ++ (show $ length $ getList $ doFolds (take 1 folds) dots)
    putStrLn $ "The answer to Part 2 is:\n" ++ (show $ doFolds folds dots)

xFold :: Int -> Dots -> Dots
xFold foldPos = Dots . nub . map movedFolded . getList
    where movedFolded dot@(x,y)
            | x < foldPos = dot
            | x > foldPos = let newX = foldPos - (x - foldPos) in (newX,y)

yFold :: Int -> Dots -> Dots
yFold foldPos = Dots . nub . map movedFolded . getList
    where movedFolded dot@(x,y)
            | y < foldPos = dot
            | y > foldPos = let newY = foldPos - (y - foldPos) in (x,newY)

doFolds :: Folds -> Dots -> Dots
doFolds [] dots = dots
doFolds (('x',pos):folds) dots = doFolds folds $ xFold pos dots
doFolds (('y',pos):folds) dots = doFolds folds $ yFold pos dots