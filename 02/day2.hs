import Data.List

main = do
    contents <- readFile "input.txt"
    let rawInstructions = map words (lines contents)
    let instructions = map parseInstruction rawInstructions
    let (position1, depth1) = part1 instructions
    let (position2, depth2, _) = part2 instructions
    putStrLn $ "The answer to Part 1 is: " ++ (show $ position1 * depth1)
    putStrLn $ "The answer to Part 2 is: " ++ (show $ position2 * depth2)

parseInstruction :: [String] -> (String, Int)
parseInstruction [command, value] = (command, read value)

part1 :: [(String, Int)] -> (Int, Int)
part1 instructions = foldl' doInstruction (0, 0) instructions
    where   doInstruction (position, depth) ("forward", x) = (position + x, depth)
            doInstruction (position, depth) ("up", x) = (position, depth - x)
            doInstruction (position, depth) ("down", x) = (position, depth + x)

part2 :: [(String, Int)] -> (Int, Int, Int)
part2 instructions = foldl' doInstruction (0, 0, 0) instructions
    where   doInstruction (position, depth, aim) ("forward", x) = (position + x, depth + x*aim, aim)
            doInstruction (position, depth, aim) ("up", x) = (position, depth, aim - x)
            doInstruction (position, depth, aim) ("down", x) = (position, depth, aim + x)
